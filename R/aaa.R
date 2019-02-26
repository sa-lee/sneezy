library(ggplot2)

# generate two 5d gaussian spheres
set.seed(1999)
p <- 5
n <- 1000
mu <- sample(c(1,2), size = n, replace = TRUE)
sphere <- matrix(rnorm(n*p, rep(mu, p)), ncol = p)
colnames(sphere) <- seq_len(p)

# run tsne
tsne <- Rtsne::Rtsne(sphere, pca = FALSE, perplexity = 60)

# plotit
plot_xy <- function(mat) { 
  colnames(mat) <- c("x", "y")  
  ggplot(as.data.frame(mat), aes(x,y)) + geom_point() 
}
# show the results
plot_xy(tsne$Y) + geom_point(aes(colour = factor(mu))) 

# find nearest neighbours based on perplexity
nn <- BiocNeighbors::findKNN(tsne$Y, k = tsne$perplexity * 3)

# generate convex hull around each neighbour
library(dplyr)
library(tidyr)
coords <- tibble(x = tsne$Y[,1],
                 y = tsne$Y[,2],
                 mu = mu,
                 inx = seq_len(n))

# flatten
edges <- nn$index
dim(edges) <- c(tsne$perplexity * 3 * n, 1)
# reshape and add in identity edges
edges <- cbind(rep(seq_len(n), each = tsne$perplexity * 3 ), 
               edges)
edges <- rbind(cbind(seq_len(n), seq_len(n)),
               edges)
colnames(edges) <- c("from", "to")
edges <- as_tibble(edges)

nodes <- left_join(edges, coords, by = c("to" = "inx")) %>%
  group_by(from) %>%
  tidyr::nest()

# we want the form that returns edges of the set for the tourr  
hulls <- nodes %>%
  mutate(hull = purrr::map(data, ~ geometry::convhulln(cbind(.$x, .$y))))

# tour
X11()
library(tourr)
animate(sphere, 
        tour_path = grand_tour(),
        display_xy(axes = "bottomleft", edges = edges)
)

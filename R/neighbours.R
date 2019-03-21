#' Generate nearest neighbours graph in t-SNE space
#' 
#' @param tnse_coords the list obtained from running `Rtsne::Rtsne()`
#' @param .subset a vector of indices to find nearest neighbours, by default
#' all nearest neighbours will be used. 
#' 
#' @return an integer matrix with columns from and to
#' 
#' @importFrom BiocNeighbors findKNN
#' 
#' @export
get_neighbourhood_graph <- function(tsne_coords, .subset = NULL) {
  # find nearest neighbours based on perplexity
  args <- norm_args_nn(tsne_coords)
  
  nn <- BiocNeighbors::findKNN(args$coords, 
                               k = args$K, 
                               subset = .subset, 
                               get.index = TRUE,
                               get.distance = FALSE)
  # flatten as an adjancey matrix
  flatten_edges(nn$index, args, .subset)

}


get_centroids_from_nn <- function(data, tsne_coords) {
  args <- norm_args_nn(tsne_coords)
  
  nn <- BiocNeighbors::findKNN(args$coords, 
                               k = args$K / 3, 
                               get.index = TRUE,
                               get.distance = FALSE)
  
  edges <- nn$index
  
  # centroids for each neighbour
  seen <- integer(nrow(edges))
  centroids <- matrix(nrow = nrow(edges), ncol = ncol(data))
  colnames(centroids) <- colnames(data)
  for (i in seq_len(nrow(edges))) {
    if (i %in% seen) {
      next
    } 
    inx <- edges[i,]
    seen <- c(i, inx, seen)
    centroids[i, ] <- colMeans(data[inx,,drop = FALSE])
  }
  
  centroids[!is.na(centroids[,1]), ]
  
  #t(apply(edges, 1, function(.x) colMeans(args$coords[.x, ])))
  
  
  
}


norm_args_nn <- function(tsne_coords) {
  list(
    coords =  tsne_coords$Y,
    K = 3 * tsne_coords$perplexity
  )
}

flatten_edges <- function(edges, args, .subset) {
  # set up dimensions
  if (!is.null(.subset)) {
    n <- length(.subset)
  } else {
    n <- nrow(args$coords)
    .subset <- seq_len(n)
  }
  # reshape to include from edges
  dim(edges) <- c(n * args$K, 1)
  edges <- cbind(rep(.subset, each = args$K), edges)
  colnames(edges) <- c("from", "to")
  edges 
}


compute_flat_dist <- function(data, coords) {
  data.frame(
    original = as.numeric(stats::dist(Rtsne::normalize_input(data))),
    embedding = as.numeric(stats::dist(Rtsne::normalize_input(coords$Y)))
  )
}

#' @export
sneezy_shep <- function(data, coords) {
  
  distances <- data.frame(D = as.numeric(stats::dist(data)),
                          d = as.numeric(stats::dist(coords$Y)))
  
  ggplot2::ggplot(data = distances, ggplot2::aes(x = d, y = D)) +
    ggplot2::geom_point() +
    ggplot2::labs(x = "Embedding distance", y = "Original distance")
  
}
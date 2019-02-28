
<!-- README.md is generated from README.Rmd. Please edit that file -->
sneezy
======

Explaining t-SNE via diagnostics and tours.

Installation
============

``` r
remotes::install_github("sa-lee/sneezy")
```

Quick start
===========

sneezy comes preloaded with some datasets from the [multi-challenge dataset](http://ifs.tuwien.ac.at/dm/dataSets.html). Let's look at a example, consisting two well sepearted Gaussian clusters in 10 dimensional space (with same covariance matrix), and two 3-dimensional Gaussian clusters embeded in 10 dimensional space. One of the clusters has more granular structure and consists of three sub clusters. From the multichallenge dataset page this subset is described as follows

> The first subset consists of a Gaussian cluster and another cluster that is itself divided into three Gaussian clusters, all of them living in a three-dimensional space. This subset is used to demonstrate how an algorithm deals with different levels of granularity in cluster structures. The distance between the centers of the two main clusters, i.e. the the big cluster and the cluster that consists of the three smaller ones, is 5 times the standard deviation d of the first main cluster. The three smaller clusters are arranged around the center of the second cluster, which they themselves form, on a circle of radius 5-d equidistant from each other. The three 3 small cluster centers and the center of the large cluster lie in the same plane. The small clusters each have a standard deviation of d and one third of the number of 3 data points of the large cluster.

Making shapes
-------------

We can get a view of the structure using principal components:

``` r
library(ggplot2)
library(gganimate) # required for printing tours
library(sneezy)
spheres <- subset(multi, key %in% c("A", "D"))
labels <- ifelse(spheres$key == "A", "sub-gaussian", "10-d 2x cluster")
spheres <- as.matrix(spheres[, -c(1,2)])

pc <- prcomp(spheres)
asp <- sqrt(pc$sdev[1] / pc$sdev[2])
ggplot(as.data.frame(pc$x), aes(PC1, PC2)) +
  geom_point(aes(colour = labels)) +
  coord_fixed(asp)
```

<img src="man/figures/README-unnamed-chunk-1-1.png" width="100%" />

And the equivalent t-SNE, with our simplified wrapper which computes exact t-SNE for a given perplexity and exaggeration factor alpha.

In this case it looks as though t-SNE has worked well: it has identified the three subclusters of the second cluster embededded in 3-dimensions, and seperated the two 10-d clusters.

``` r
set.seed(1010010)
coords <- basic_tsne(spheres, perplexity = 30)
pl <- ggplot(as.data.frame(coords$Y), aes(V1, V2)) +
  geom_point(aes(colour = labels)) +
  coord_fixed(asp) +
  scale_color_brewer(palette = "Dark2")
pl
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

We can also tour around a data space, and see how the nearest neighbours graph from t-SNE space is preserved in high-dimensional space. We can take subsets of the nn graph to see how the t-SNE preserves local topology, for example the points on the outside of the subcluster on the right hand side:

``` r
pal <- c("#1B9E77", "#D95F02")[as.integer(as.factor(labels))]
sneezy_neighbours(spheres, coords, .subset = 171, col = pal)
#> Using half_range 1.2
```

<img src="man/figures/README-unnamed-chunk-3-1.gif" width="100%" />

We can also triangulate the points in t-SNE space, and see how that moves via the grand tour.

``` r
tri <- get_triangles(coords)
Y_df <- as.data.frame(coords$Y)
mesh <- data.frame(x = Y_df[tri[,1], 1],
                   y = Y_df[tri[, 1], 2],
                   xend = Y_df[tri[,2], 1],
                   yend = Y_df[tri[,2], 2])

pl +  geom_segment(data = mesh, aes(x =x, xend = xend, y = y, yend =yend))
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

``` r
sneezy_triangles(spheres, coords, col = pal)
#> Using half_range 1.2
```

<img src="man/figures/README-unnamed-chunk-5-1.gif" width="100%" />

And look at the centroids in the original space of the nearest neighbours graph in t-SNE space:

``` r
sneezy_centroids(spheres, coords)
#> Using half_range 1.2
```

<img src="man/figures/README-unnamed-chunk-6-1.gif" width="100%" />

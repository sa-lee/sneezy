#' Overlay centroids via k-means or graph-based clustering
#' @inheritParams overlay-neighbors
#' @export
#' @name overlay-centroids
#' @rdname overlay-centroids
#' @details The nearest neighbor centroids functions
#' automatically estimate centroids from an indices matrix 
#' representing a nearest neighbor graph via a community detection
#' algorithm (louvain) from the [igraph] package. 
#' The smaller the value of `ncol(indices)` the more centroids
#' there will be. The kmeans method returns the centroids from
#' run [stats::kmeans] with the user suppplied number of centers.
#' 
#' @return 
#' a [ggplot2::geom_point] layer
overlay_knn_centroids <- function(x, y, indices, ...) {
  vals <- estimate_knn_centroids(x,y,indices)
  ggplot2::geom_point(data = vals, ggplot2::aes(x = x, y = y), ...)
}


#' @inheritParams overlay-neighbors
#' @export
#' @name overlay-centroids
#' @rdname overlay-centroids
overlay_snn_centroids <- function(x, y, indices, ...) {
  vals <- estimate_snn_centroids(x,y,indices)
  ggplot2::geom_point(data = vals, ggplot2::aes(x = x, y = y), ...)
}


#' Overylay k-means centroids onto an xy scatter
#' @inheritParams overlay-neighbors
#' @param num_centers the number of centroids to estimate
#' @export
#' @name overlay-centroids
#' @rdname overlay-centroids
#' @importFrom stats kmeans
overlay_kmeans_centroids <- function(x, y, num_centers, ...) {
  vals <- as.data.frame(estimate_kmeans(x, y, num_centers)$centers)
  ggplot2::geom_point(data = vals, ggplot2::aes(x = x, y = y), ...)
}



# Compute group-wise centroids for vectors x,y 
centroids_by_groups <- function(groups, x, y) {
  centroids <- lapply(groups, function(inx) {
    x <- mean(x[inx])
    y <- mean(y[inx])
    c(x = x, y = y)
  })
  as.data.frame(do.call("rbind", centroids))
}

# Compute group-wise column means for a matrix
centroids_by_groups_mat <- function(groups, mat) {
  centroids <- lapply(groups, 
                      function(inx) {
                        colMeans(mat[inx,])
                      })
  do.call("rbind", centroids)
}


estimate_knn_centroids <- function(x, y, indices) {
  knn <- scran::neighborsToKNNGraph(indices)
  clust <- igraph::cluster_louvain(knn)
  groups <- igraph::communities(clust)
  centroids_by_groups(groups, x, y)
}

estimate_snn_centroids <- function(x, y, indices) {
  snn <- scran::neighborsToSNNGraph(indices)
  clust <- igraph::cluster_louvain(snn)
  groups <- igraph::communities(clust)
  centroids_by_groups(groups, x, y)
}

#' @importFrom stats kmeans
estimate_kmeans <- function(x, y, num_centers) {
  stats::kmeans(cbind(x = x, y = y), centers = num_centers)
}
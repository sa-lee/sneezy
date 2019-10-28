#' Estimate k-nearest neighbours graph from a TourExperiment object
#' 
#' @param .data a `TourExperiment` object 
#' @param num_neighbors An integer scalar for number of neighbors
#' @param .on a character scalar, indicating the part of `.data` to
#' estimate nearest neighbors. If missing the first assay will be used. 
#' @param .engine A `BiocNeighbor::BiocNeighborParam()` object that
#' reperesents the algorithm used to compute nearest neighbors. 
#' 
#' @details The representation of the nearest neighbors is a matrix of
#' size ncol(.data) by num_neighbors containing the neighbor indexes for
#' each sample in `.data`
#'
#' @return A `TourExperiment` object with the `neighborSets` slot
#' filled. 
#' @importFrom BiocNeighbors findKNN KmknnParam
#' 
#' @export
estimate_neighbors <- function(.data, num_neighbors, .on = NULL, .engine = BiocNeighbors::KmknnParam()) {
  
  if (!is(.data, "TourExperiment")) {
    stop("`.data` must be a TourExperiment object")
  }
  
  if (!(is(.on, "character") || is.null(.on))) {
    stop("`.on` must be a character(1) vector or NULL")
  }
  
  val <- .retrieve_mat(.data, .on)
  
  nn <- BiocNeighbors::findKNN(val, 
                               k = num_neighbors, 
                               get.index = TRUE,
                               get.distance = FALSE,
                               BNPARAM = .engine)
  neighborSet(.data, .on) <- nn$index
  .data
}


#' Overlay a neighborSet on a set of xy coordinates
#' 
#' @param x,y numeric vectors to produce segments from
#' @param indices a neighborSet index matrix
#' @param ... additional arguments to pass to [ggplot2::geom_segment]
#' 
#' @export
#' @importFrom ggplot2 geom_segment
overlay_neighbors <- function(x, y, indices, ...) {
  
  flattened <- flatten_graph(indices)
  mesh <- create_mesh(x,y, flattened)
  add_segments(mesh, 
               ggplot2::vars(xend = xend, yend = yend),
               ...)
}


#' Overlay a shared nearest neighbors graph onto an xy-scatter
#' 
#' @inheritParams overlay_neighbors
#' @param type Algorithm used to compute shared nearest neighbors graph
#' @export 
#' @importFrom scran neighborsToSNNGraph neighborsToKNNGraph
#' @importFrom igraph get.data.frame
overlay_shared_neighbors <- function(x, y, indices, type = "rank", ...) {
  flattened <- scran::neighborsToSNNGraph(indices, type = type)
  flattened <- igraph::get.data.frame(flattened,what = "edges")
  mesh <- cbind(create_mesh(x,y, flattened), 
                weight = flattened[["weight"]])
  add_segments(mesh,
               ggplot2::vars(xend = xend, yend = yend, alpha = weight),
               ...)
  
}

create_mesh <- function(x, y, flattened) {
  data.frame(x = x[flattened[,1]],
             y = y[flattened[,1]],
             xend = x[flattened[,2]],
             yend = y[flattened[,2]])
}

add_segments <- function(mesh, aes, ...) {
  ggplot2::geom_segment(
    data = mesh, 
    ggplot2::aes(x, y, !!!aes),
    ...
  )
}

centroids_by_groups <- function(groups, x, y) {
  centroids <- lapply(groups, function(inx) {
    x <- mean(x[inx])
    y <- mean(y[inx])
    c(x = x, y = y)
  })
  as.data.frame(do.call("rbind", centroids))
}

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

estimate_kmeans <- function(x, y, num_centers) {
  stats::kmeans(cbind(x = x, y = y), centers = num_centers)
}

#' Overlay k-NN centroids
#' @inheritParams overlay_neighbors
#' @export
overlay_knn_centroids <- function(x, y, indices, ...) {
  vals <- estimate_knn_centroids(x,y,indices)
  ggplot2::geom_point(data = vals, ggplot2::aes(x = x, y = y), ...)
}


#' Overlay s-NN centroids
#' @inheritParams overlay_neighbors
#' @export
overlay_snn_centroids <- function(x, y, indices, ...) {
  vals <- estimate_snn_centroids(x,y,indices)
  ggplot2::geom_point(data = vals, ggplot2::aes(x = x, y = y), ...)
}


#' Overylay k-means centroids onto an xy scatter
#' @inheritParams overlay_neighbors
#' @param num_centers the number of centroids to estimate
#' @export
#' @importFrom stats kmeans
overlay_kmeans_centroids <- function(x, y, num_centers, ...) {
  vals <- as.data.frame(estimate_kmeans(x, y, num_centers)$centers)
  ggplot2::geom_point(data = vals, ggplot2::aes(x = x, y = y), ...)
}
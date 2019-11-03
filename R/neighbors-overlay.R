#' Overlay a neighborSet onto a set of xy coordinates
#' 
#' @param x,y numeric vectors to produce segments from
#' @param indices a neighborSet index matrix
#' @param ... additional arguments to pass to [ggplot2::geom_segment]
#' 
#' @export
#' @importFrom ggplot2 geom_segment
#' @name overlay-neighbors
#' @rdname overlay-neighbors
overlay_neighbors <- function(x, y, indices, ...) {
  mesh <- .create_mesh(x,y, indices)
  add_segments(mesh, 
               ggplot2::vars(xend = xend, yend = yend),
               ...)
}

#' @inheritParams overlay_neighbors
#' @param type Algorithm used to compute shared nearest neighbors graph
#' @export 
#' @seealso [scran::neighborsToSNNGraph]
#' @importFrom scran neighborsToSNNGraph neighborsToKNNGraph
#' @importFrom igraph get.data.frame
#' @export
#' @name overlay-neighbors
#' @rdname overlay-neighbors
overlay_shared_neighbors <- function(x, y, indices, type = "rank", ...) {
  mesh <- .create_mesh(x,y, indices, as = "snn", type =  type)
  add_segments(mesh,
               ggplot2::vars(xend = xend, yend = yend, alpha = weight),
               ...)
  
}

.create_mesh <- function(x, y, indices, as = "knn", ...) {
  as <- match.arg(as, c("knn", "snn"))
  if (as == "knn") {
    edges <- igraph::get.data.frame(
      scran::neighborsToKNNGraph(indices), 
      what = "edges"
    )
    mesh <- .expand_by_edges(x, y, edges)
  } else {
    edges <- igraph::get.data.dframe(
      scran::neighborsToSNNGraph(indices, ...),
      what = "edges"
    )
    mesh <- .expand_by_edges(x, y, edges)
    mesh[["weight"]] <- edges[["weight"]]
  }
  
}

.expand_by_edges <- function(x, y, edges) {
  data.frame(
    x = x[edges[,1]],
    y = y[edges[,1]],
    xend = x[edges[,2]],
    yend = y[edges[,2]]
  )
}

add_segments <- function(mesh, aes, ...) {
  ggplot2::geom_segment(
    data = mesh, 
    ggplot2::aes(x, y, !!!aes),
    ...
  )
}

overlay_difference_neighbors <- function(x, y, set1, set2, ...) {
  g1 <- scran::neighborsToKNNGraph(set1)
  g2 <- scran::neighborsToKNNGraph(set2)
  igraph::difference(g1, g2)
}
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

#' Overlay a shared nearest neighbors graph onto an xy-scatter
#' 
#' @inheritParams overlay_neighbors
#' @param type Algorithm used to compute shared nearest neighbors graph
#' @export 
#' @seealso [scran::neighborsToSNNGraph]
#' @importFrom scran neighborsToSNNGraph neighborsToKNNGraph
#' @importFrom igraph get.data.frame
#' @export
overlay_shared_neighbors <- function(x, y, indices, type = "rank", ...) {
  flattened <- scran::neighborsToSNNGraph(indices, type = type)
  flattened <- igraph::get.data.frame(flattened,what = "edges")
  mesh <- cbind(create_mesh(x,y, flattened), 
                weight = flattened[["weight"]])
  add_segments(mesh,
               ggplot2::vars(xend = xend, yend = yend, alpha = weight),
               ...)
  
}
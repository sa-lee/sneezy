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

#' @inheritParams overlay-neighbors
#' @param type Algorithm used to compute shared nearest neighbors graph
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
    edges <- igraph::get.data.frame(
      scran::neighborsToSNNGraph(indices, ...),
      what = "edges"
    )
    mesh <- .expand_by_edges(x, y, edges)
    mesh[["weight"]] <- edges[["weight"]]
  }
  mesh
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

.graph_op <- function(set1, set2, as = "knn", op = igraph::difference, ...) {
  as <- match.arg(as, c("knn", "snn"))
  if (as == "knn") {
    g1 <- scran::neighborsToKNNGraph(set1)
    g2 <- scran::neighborsToKNNGraph(set2)
  } else {
    g1 <- scran::neighborsToSNNGraph(set1, ...)
    g2 <- scran::neighborsToSNNGraph(set2, ...)
  }
  op(g1, g2)
}

#' Overlay a difference/intersection of two neighborSets
#' 
#' @param x,y numeric vectors to produce segments from
#' @param set1,set2 nearest neighbor graph indices to compare
#' 
#' @name overlay-neighbors-diff
#' @rdname overlay-neighbors-diff
#' @export
overlay_difference_neighbors <- function(x, y, set1, set2, ...) {
  edges <- igraph::get.data.frame(.graph_op(set1, set2))
  mesh <- .expand_by_edges(x,y,edges)
  add_segments(mesh, 
               ggplot2::vars(xend = xend, yend = yend),
               ...)
}

#' @inheritParams overlay-neighbors-diff
#' @param type Algorithm used to compute shared nearest neighbors graph
#' @name overlay-neighbors-diff
#' @rdname overlay-neighbors-diff
#' @export
overlay_difference_shared_neighbors <- function(x, y, set1, set2, type = "rank", ...) {
  edges <- igraph::get.data.frame(.graph_op(set1, set2, as = "snn", type = type))
  mesh <- .expand_by_edges(x,y,edges)
  add_segments(mesh, 
               ggplot2::vars(xend = xend, yend = yend, alpha = weight),
               ...)
}


#' @inheritParams overlay-neighbors-diff
#' @name overlay-neighbors-diff
#' @rdname overlay-neighbors-diff
#' @export
overlay_intersect_neighbors <- function(x, y, set1, set2, ...) {
  edges <- igraph::get.data.frame(.graph_op(set1, set2, 
                                            op = igraph::intersection))
  mesh <- .expand_by_edges(x,y,edges)
  add_segments(mesh, 
               ggplot2::vars(xend = xend, yend = yend),
               ...)
}


#' @inheritParams overlay-neighbors-diff
#' @name overlay-neighbors-diff
#' @rdname overlay-neighbors-diff
#' @export
overlay_intersect_shared_neighbors <- function(x, y, set1, set2, type = "rank", ...) {
  edges <- igraph::get.data.frame(.graph_op(set1, set2,
                                            as = "snn",
                                            op = igraph::intersection,
                                            type = type))
  mesh <- .expand_by_edges(x,y,edges)
  add_segments(mesh, 
               ggplot2::vars(xend = xend, yend = yend),
               ...)
}
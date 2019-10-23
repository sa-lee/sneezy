#' Esiimate k-nearest neighbours graph from a TourExperiment object
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
    stop(".data is not a TourExperiment object")
  }
  
  if (!(is(.on, "character") || is.null(.on))) {
    stop("from must be a character(1) or NULL")
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
  
  flattened <- flatten_graph(indices, .subset)
  
  mesh <- data.frame(x = x[flattened[,1]],
                     y = y[flattened[,1]],
                     xend = x[flattened[,2]],
                     yend = y[flattened[,2]])
  ggplot2::geom_segment(data = mesh, 
                        ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                        ...)
}


.retrieve_mat <- function(.data, from = NULL, .subset = NULL) {
  
  if (is.null(from)) {
    res <- t(assay(.data))
  } else {
    # check slots
    # idea is to get named assay unless there's a reducedDim slot
    # if there isn't then throw an error
    a_selector <- intersect(from, SummarizedExperiment::assayNames(.data))
    if (length(a_selector) == 0) {
      rd_selector <- intersect(from, SingleCellExperiment::reducedDimNames(.data))
      if (length(rd_selector) == 0) {
        stop(paste("`from`:", from, "is not available in object."))
      }
      # subset is ignored if returning a reducedDim slot
      return(SingleCellExperiment::reducedDim(.data, rd_selector))
    } else {
      
      res <- t(SummarizedExperiment::assay(.data, a_selector))
    }
  }
  
  if (!is.null(.subset)) {
    return(res[, .subset])
  }
  res
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

flatten_graph <- function(indices) {
  n <- nrow(indices)
  .subset <- seq_len(n)
  k <- ncol(indices)
  # reschape into from/to form
  dim(indices) <- c(n * k, 1)
  indices <- cbind(rep(.subset, each =  k), indices)
  colnames(indices) <- c("from", "to")
  indices
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


#' Animate grand tour with t-SNE neighbourhood graph
#' 
#' 
#' @param data a numeric dataset
#' @param tsne_coords a list from running `Rtsne::Rtsne()`
#' @param .subset a vector of indices to find nearest neighbours, by default
#' all nearest neighbours will be used.
#' @param ... other control options passed to [tourr::display_xy()]
#' 
#' @export
sneezy_neighbours <- function(data, tsne_coords, .subset = NULL, ...) {
  nn_graph <- get_neighbourhood_graph(tsne_coords, .subset)
  gif_tour(data, edges = nn_graph, ...)
  
}

#' @export
sneezy_centroids <- function(data, tsne_coords, col, ...) {
  centroids <- get_centroids_from_nn(data, tsne_coords)
  col <- scales::alpha(c(col, rep("red", nrow(centroids))))
  data <- rbind(data, centroids)
  gif_tour(data, tour_path = tourr::grand_tour(), edges = NULL, col = col)
  
}
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

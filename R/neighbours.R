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
  perpelexity <- tsne_coords$perplexity
  coords <- tsne_coords$Y
  
  K <- 3 * perpelexity
  
  nn <- BiocNeighbors::findKNN(coords, 
                               k = K, 
                               subset = .subset, 
                               get.index = TRUE,
                               get.distance = FALSE)
  # flatten as an adjancey matrix
  edges <- nn$index
  
  # set up dimensions
  if (!is.null(.subset)) {
    n <- length(.subset)
  } else {
    n <- nrow(coords)
    .subset <- seq_len(n)
  }
  # reshape to include from edges
  dim(edges) <- c(n * K, 1)
  edges <- cbind(rep(.subset, each = K), edges)
  colnames(edges) <- c("from", "to")
  edges 
}



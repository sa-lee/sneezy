#' Generate nearest neighbours graph in t-SNE space
#' 
#' @param tnse_coords the list obtained from running `Rtsne::Rtsne()`
#' 
#' @export
get_neighbourhood_graph <- function(tsne_coords) {
  # find nearest neighbours based on perplexity
  perpelexity <- tsne_coords$perplexity
  coords <- tsne_coords$Y
  
  n <- nrow(coords)
  K <- 3 * perpelexity
  
  nn <- BiocNeighbors::findKNN(coords, k = K)
  
  # flatten as an adjancey matrix
  edges <- nn$index
  dim(edges) <- c(n * K, 1)
  # reshape and add in identity edges
  edges <- cbind(rep(seq_len(n), each = K), 
                 edges)
  edges <- rbind(cbind(seq_len(n), seq_len(n)),
                 edges)
  colnames(edges) <- c("from", "to")
  edges 
}

#' Simplified running of t-SNE
#' 
#' @details This a very minimal wrapper to `Rtsne::Rtsne()`
#' that computes exact t-SNE for a given dataset, withouth
#' normalising the input data or running PCA beforehand.
#' 
#' @param data a data.frame or matrix to compute t-SNE on
#' @param perplexity the perpelxity paramater to t-SNE algorithm (see `Rtsne::Rtsne()`)
#' 
#' 
#' @export
basic_tsne <- function(data, perplexity ) {
  Rtsne::Rtsne(data,
               perplexity = perplexity,
               theta = 0,
               pca = FALSE,
               normalize_input = FALSE)
}

#' Animate grand tour with t-SNE neighbourhood graph
#' 
#' 
#' @param data a numeric dataset
#' @param tsne_coords a list from running `Rtsne::Rtsne()`
#' 
#' @export
view_tour <- function(data, tsne_coords) {
  
  nn_graph <- get_neighbourhood_graph(tsne_coords)
  
  tourr::animate(data, 
          tour_path = tourr::grand_tour(),
          tourr::display_xy(axes = "bottomleft", edges = nn_graph)
  )
}

#' Simplified grand tour history
#' 
#' @export
basic_tour_path <- function(data) {
  projections <- tourr::save_history(data, rescale = FALSE)
  attr(projections, "data") <- NULL
  class(projections) <- NULL
  projections
}
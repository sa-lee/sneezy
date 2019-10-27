#' Animate tour with neighborhood graph
#' 
#' 
#' @param .data a numeric dataset
#' @param basis name of basisSet in .data
#' @param neighbor name of neighborSet in .data 
#' @param .subset a vector of indices to find nearest neighbours, by default
#' all nearest neighbours will be used.
#' @param ... other control options passed to [tourr::display_xy()]
#' 
#' @export
sneezy_neighbors <- function(.data, basis, neighbor, ...) {
  # extract basisSet
  projs <- basisSet(.data, basis)
  # flatten to array
  projs <- flatten_array(projs)
  plan <- tourr::planned_tour(projs)
  
  # extract neighborSet
  n_set <- neighborSet(.data, neighbor)
  n_set <- flatten_graph(n_set)
  
  # named basisSet will be .data
  vals <- .retrieve_mat(.data, basis)

  # set up display
  dxy <- tourr::display_xy(edges = n_set, ...)
  tourr::render_gif(vals, tour_path = plan)
}



#' @export
sneezy_centroids <- function(data, tsne_coords, col, ...) {
  centroids <- get_centroids_from_nn(data, tsne_coords)
  col <- scales::alpha(c(col, rep("red", nrow(centroids))))
  data <- rbind(data, centroids)
  gif_tour(data, tour_path = tourr::grand_tour(), edges = NULL, col = col)
  
}
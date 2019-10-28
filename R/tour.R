#' Animate tour with neighborhood graph
#' 
#' 
#' @param .data a numeric dataset
#' @param basis name of basisSet in .data
#' @param neighbor name of neighborSet in .data 
#' @param ... other control options passed to [tourr::display_xy()]
#' 
#' @export
#' @importFrom tourr planned_tour render_gif 
#' @importFrom scran neighborsToSNNGraph
#' @importFrom igraph cluster_louvain communities
sneezy_neighbors <- function(.data, basis, neighbor, ...) {
  
  stopifnot(is(.data, "TourExperiment"))
  
  plan <- .setup_plan(.data, basis)
  
  # extract neighborSet
  n_set <- neighborSet(.data, neighbor)
  n_set <- flatten_graph(n_set)
  
  # named basisSet will be .data
  vals <- .retrieve_mat(.data, basis)
  # set up display
  dxy <- tourr::display_xy(edges = n_set, ...)
  
  gganimate::gif_file(
    tourr::render_gif(vals, 
                      tour_path = plan, 
                      display = dxy)
  )
}


#' Animate a tour with a centroids overlay
#' 
#' @inheritParams sneezy_neighbors
#' @export
sneezy_centroids <- function(.data, basis, neighbor, ...) {
  
  plan <- .setup_plan(.data, basis)
  
  vals <- .retrieve_mat(.data, basis)
  
  indices <- neighborSet(.data, neighbor)
  snn <- scran::neighborsToSNNGraph(indices)
  clust <- igraph::cluster_louvain(snn)
  groups <- igraph::communities(clust)
  centroids <- centroids_by_groups_mat(groups, vals)
  
  centroids.col <- rep("red", nrow(centroids))
  
  extra_args <- list(...)
  
  if ("col" %in% names(extra_args)) {
    col <- scales::alpha(c(extra_args[["col"]], 
                           rep("red", nrow(centroids))))
  } else {
    col <- scales::alpha(rep("black", nrow(vals)),  centroids.col)
  }
  
  vals <- rbind(vals, centroids)
  
  # set up display
  dxy <- tourr::display_xy(col = col)
  
  gganimate::gif_file(
    tourr::render_gif(vals, 
                      tour_path = plan, 
                      display = dxy
    )
  )
  
}

.setup_plan <- function(.data, basis) {
  # extract basisSet
  projs <- basisSet(.data, basis)
  # flatten to array
  projs <- flatten_array(projs)
  tourr::planned_tour(projs)
}
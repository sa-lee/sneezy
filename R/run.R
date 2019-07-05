#' Simplified running of t-SNE
#' 
#' @details This a very minimal wrapper to `Rtsne::Rtsne()`
#' that computes exact t-SNE for a given dataset, withouth
#' normalising the input data or running PCA beforehand.
#' 
#' @param data a data.frame or matrix to compute t-SNE on
#' @param perplexity the perpelxity paramater to t-SNE algorithm (see [Rtsne::Rtsne()])
#' @param alpha the exaggeration factor for the intial part of the optimsation 
#' 
#' @seealso 
#' [Rtsne::Rtsne()]
#' @export
basic_tsne <- function(data, perplexity, alpha = nrow(data) / 10) {
  Rtsne::Rtsne(data,
               perplexity = perplexity,
               exaggeration_factor = alpha,
               theta = 0,
               pca = FALSE,
               normalize = TRUE)
}

sneezy_tsne <- function(data, perplexity, alpha = nrow(data) / 10, theta = 0.5,  n_dims = min(3, ncol(data))) {
  Rtsne::Rtsne(data,
               perplexity = perplexity,
               exaggeration_factor = alpha,
               theta = theta,
               dims = n_dims,
               normalize = TRUE)
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
sneezy_triangles <- function(data,  tsne_coords, ...) {
  edges <- get_triangles(tsne_coords)
  alpha <- 1/ nrow(edges)^(1/3)
  edges.col <- scales::alpha("black", alpha)
  gif_tour(data, edges = edges, ..., edges.col = edges.col)
}



#' @export
sneezy_centroids <- function(data, tsne_coords) {
  centroids <- get_centroids_from_nn(data, tsne_coords)
  col <- c(rep("black", nrow(data)), rep("red", nrow(centroids)))
  data <- rbind(data, centroids)
  gif_tour(data, edges = NULL, col = col)
  
}
#' Simplified grand tour history
#' 
#' @export
basic_tour_path <- function(data, max_bases) {
  projections <- tourr::save_history(data, max_bases = max_bases)
  attr(projections, "data") <- NULL
  class(projections) <- NULL
  projections
}



gif_tour <- function(data, tour_path = tourr::grand_tour(), edges = NULL, ... ) {
  dir <- tempdir()
  png_path <- file.path(dir, "frame%03d.png")
  
  tourr::render(data,
                tour_path = tour_path,
                display = tourr::display_xy(axes = "bottomleft", 
                                            edges = edges,
                                            ...),
                dev = "png",
                png_path,
                frames = as.integer(ncol(data)^sqrt(ncol(data) / 2)),
                rescale = TRUE
  )
  png_files <- sprintf(png_path, 1:100)
  gif_file <- tempfile(fileext = ".gif")
  gifski::gifski(png_files, gif_file, delay = 0.25, progress = FALSE)
  on.exit(unlink(png_files))
  gganimate::gif_file(gif_file)
}
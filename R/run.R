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
#' @param .subset a vector of indices to find nearest neighbours, by default
#' all nearest neighbours will be used.
#' 
#' 
#' @export
view_tour <- function(data, tsne_coords, .subset = NULL) {
  
  nn_graph <- get_neighbourhood_graph(tsne_coords, .subset)
  gif_tour(data, nn_graph)
  
}

gif_tour <- function(data, edges, ...) {
  dir <- tempdir()
  png_path <- file.path(dir, "frame%03d.png")
  
  tourr::render(data,
                tour_path = tourr::grand_tour(),
                display = tourr::display_xy(axes = "bottomleft", 
                                            edges = edges,
                                            ...),
                dev = "png",
                png_path,
                frames = 100
  )
  png_files <- sprintf(png_path, 1:100)
  gif_file <- tempfile(fileext = ".gif")
  gifski::gifski(png_files, gif_file, delay = 0.25, progress = FALSE)
  on.exit(unlink(png_files))
  gganimate::gif_file(gif_file)
}

#' @export
view_tour_nn_centroids <- function(data, tsne_coords) {
  centroids <- get_centroids_from_nn(data, tsne_coords)
  col <- c(rep("black", nrow(data)), rep("red", nrow(centroids)))
  data <- rbind(data, centroids)
  gif_tour(data, edges = NULL, col = col)
  
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
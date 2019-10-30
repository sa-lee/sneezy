#' Animate a tour path with a neighborhood overlay
#' 
#' 
#' @param .data a numeric dataset
#' @param basis name of basisSet in .data
#' @param neighbor name of neighborSet in .data 
#' @param apf angles per frame (defulats to 1/30)
#' @param frames number of frames in animation (defaults to 300)
#' @param height,width the size of the animation in pixes 
#' @param ... other control options passed to [tourr::display_xy()]
#' 
#' @return 
#' A [gganimate::gif_file] that can be embedded in an Rmd or 
#' R console viwer pane.
#' 
#' @export
#' @importFrom tourr planned_tour render_gif 
#' @importFrom scran neighborsToSNNGraph
#' @importFrom igraph cluster_louvain communities
#' @importFrom gifski gifski
#' @importFrom gganimate gif_file
#' 
#' @seealso [tourr::animate_xy()], [tourr::display_xy()]
sneezy_neighbors <- function(.data, basis, neighbor, apf = 1/10, frames = 100, height = 400, width = 400, ...) {
  
  stopifnot(is(.data, "TourExperiment"))
  stopifnot(is.character(basis) && length(basis) == 1L)
  stopifnot(is.character(neighbor) && length(neighbor) == 1L)
  
  # tour path
  plan <- .setup_plan(.data, basis)
  
  # extract neighborSet
  n_set <- neighborSet(.data, neighbor)
  n_set <- flatten_graph(n_set)
  
  # named basisSet will be .data
  vals <- .retrieve_mat(.data, basis)
  
  extra_args <- list(...)
  
  if (!("axes" %in% names(extra_args))) {
    extra_args[["axes"]] <- "bottomleft"
  }
  
  extra_args[["edges"]] <- n_set
  
  
  # set up display
  dxy <- do.call(tourr::display_xy, extra_args)
  
  .gif_tour(vals, plan, dxy, apf = apf, frames = frames, height = height, width = width)
}


#' Animate a tour path with a centroids overlay
#' 
#' 
#' @inheritParams sneezy_neighbors
#' @param centroids.col A character(1) with a color name for the centroid points (default = "black")
#' 
#' @export
sneezy_centroids <- function(.data, basis, neighbor, centroid.col = "black", apf = 1/10, frames = 100, height = 400, width = 400, ...) {
  
  plan <- .setup_plan(.data, basis)
  
  vals <- .retrieve_mat(.data, basis)
  
  indices <- neighborSet(.data, neighbor)
  snn <- scran::neighborsToSNNGraph(indices)
  clust <- igraph::cluster_louvain(snn)
  groups <- igraph::communities(clust)
  centroids <- centroids_by_groups_mat(groups, vals)
  
  centroids.col <- rep(centroid.col, nrow(centroids))
  
  
  # if colour is defined by user, we we
  extra_args <- list(...)
  
  if ("col" %in% names(extra_args)) {
    extra_args[["col"]] <- c(extra_args[["col"]], centroids.col)
  } else {
    extra_args[["col"]] <- c(rep("grey50", nrow(vals)),  centroids.col)
  }
  
  if (!("axes" %in% names(extra_args))) {
    extra_args[["axes"]] <- "bottomleft"
  }
  
  vals <- rbind(vals, centroids)
  
  # set up display
  dxy <- do.call(tourr::display_xy, extra_args)

  .gif_tour(vals, plan, dxy, apf, frames, width, height)
  
}

.setup_plan <- function(.data, basis) {
  # extract basisSet
  projs <- basisSet(.data, basis)
  # flatten to array
  projs <- flatten_array(projs)
  tourr::planned_tour(projs)
}

# internal function for rendering a tour path in rstudio or knitr
.gif_tour <- function(data, tour_path, display, apf, frames, width, height) {
  dir <- tempdir()
  png_path <- file.path(dir, "frame%03d.png")
  
  tourr::render(data,
                tour_path = tour_path,
                display = display,
                dev = "png",
                png_path,
                apf = apf,
                frames = frames,
                width = width,
                height = height
  )
  
  png_files <- sprintf(png_path, seq_len(frames))
  gif_file <- tempfile(fileext = ".gif")
  gifski::gifski(png_files, gif_file, delay = apf, height = height, width = width)
  on.exit(unlink(png_files))
  gganimate::gif_file(gif_file)
  
}

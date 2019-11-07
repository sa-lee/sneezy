#' Animate a tour path directly with TourExperiment
#' 
#' @param .data A [TourExperiment] object  
#' @param basis The name or position of a [basisSet] in `.data`, 
#' defaults to the first element. The name of the [basisSet] will
#' also be used to extract the underlying data. 
#' @param row_subset The number of rows to subset the view on, the default
#' which is NULL will select all rows.
#' @param apf angles per frame (defaults to 1/30)
#' @param frames number of frames in animation (defaults to 300)
#' @param height,width the size of the animation in pixels 
#' @param ... other control options passed to [tourr::display_xy()]
#' 
#' @return 
#' A [gganimate::gif_file] that can be embedded in an Rmd or 
#' R console viwer pane.
#' 
#' @export
#' @examples 
#' multi_te <- TourExperiment(multi, X1:X10)
#' multi_te <- generate_bases(multi_te, .on = "view")
#' multi_te <- estimate_neighbors(multi_te, 10, .on = "view")
#' 
#' sneezy_neighbors(multi_te)
#' 
#' @seealso [tourr::animate_xy()], [tourr::display_xy()]
sneezy_tour <- function(.data, basis = 1, row_subset = NULL, apf = 1/10, frames = 100, height = 400, width = 400, ...) {
  
  .norm_args_sneezy(.data, basis, neighbor = 1)
  
  basis <- .set_basis(.data, basis)
  
  # tour path
  plan <- .setup_plan(.data, basis)
  
  # figure out if we need to subset columns
  # column subset
  col_subset <- seq_len(nrow(basisSet(.data, basis)))
  
  # named basisSet will be .data
  vals <- .retrieve_mat(.data, basis, .subset = row_subset)
  
  vals <- vals[, col_subset, drop = FALSE]
  if (is(vals, "LinearEmbeddingMatrix")) {
    vals <- sampleFactors(vals)
  }
  
  extra_args <- list(...)
  
  if (!("axes" %in% names(extra_args))) {
    extra_args[["axes"]] <- "bottomleft"
  }
  
  
  # set up display
  dxy <- do.call(tourr::display_xy, extra_args)
  
  .gif_tour(vals, plan, dxy, 
            apf = apf, frames = frames, height = height, width = width)
}


#' Animate a tour path with a neighborhood overlay
#' 
#' @param .data A [TourExperiment] object  
#' @param basis The name or position of a [basisSet] in `.data`, 
#' defaults to the first element. The name of the [basisSet] will
#' also be used to extract the underlying data. 
#' @param neighbor The name or position of a [neighborSet] in `.data`,
#' defaults to the first element. 
#' @param row_subset The number of rows to subset the view on, the default
#' which is NULL will select all rows.
#' @param apf angles per frame (defaults to 1/30)
#' @param frames number of frames in animation (defaults to 300)
#' @param height,width the size of the animation in pixels 
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
#' @examples 
#' multi_te <- TourExperiment(multi, X1:X10)
#' multi_te <- generate_bases(multi_te, .on = "view")
#' multi_te <- estimate_neighbors(multi_te, 10, .on = "view")
#' 
#' sneezy_neighbors(multi_te)
#' 
#' @seealso [tourr::animate_xy()], [tourr::display_xy()]
sneezy_neighbors <- function(.data, basis = 1, neighbor = 1, row_subset = NULL, apf = 1/10, frames = 100, height = 400, width = 400, ...) {
  
  .norm_args_sneezy(.data, basis, neighbor)
  
  basis <- .set_basis(.data, basis)
  
  # tour path
  plan <- .setup_plan(.data, basis)
  
  # figure out if we need to subset columns
  # column subset
  col_subset <- seq_len(nrow(basisSet(.data, basis)))
  
  # extract neighborSet
  n_set <- neighborSet(.data, neighbor)
  n_set <- flatten_graph(n_set)
  
  # named basisSet will be .data
  vals <- .retrieve_mat(.data, basis, row_subset)
  vals <- vals[, col_subset, drop = FALSE]
  if (is(vals, "LinearEmbeddingMatrix")) {
    vals <- sampleFactors(vals)
  }
  
  extra_args <- list(...)
  
  if (!("axes" %in% names(extra_args))) {
    extra_args[["axes"]] <- "bottomleft"
  }
  
  extra_args[["edges"]] <- n_set
  
  
  # set up display
  dxy <- do.call(tourr::display_xy, extra_args)
  
  .gif_tour(vals, plan, dxy, 
            apf = apf, frames = frames, height = height, width = width)
}


#' Animate a tour path with a centroids overlay
#' 
#' 
#' @inheritParams sneezy_neighbors
#' @param centroids.col A character(1) with a color name for the centroid points (default = "black")
#' @examples 
#' multi_te <- TourExperiment(multi, X1:X10)
#' multi_te <- generate_bases(multi_te, .on = "view")
#' multi_te <- estimate_neighbors(multi_te, 10, .on = "view")
#' 
#' sneezy_centroids(multi_te)
#' @export
sneezy_centroids <- function(.data, basis = 1, neighbor = 1, row_subset = NULL, centroid.col = "black", apf = 1/10, frames = 100, height = 400, width = 400, ...) {
  
  .norm_args_sneezy(.data, basis, neighbor)
  
  basis <- .set_basis(.data, basis)
  
  plan <- .setup_plan(.data, basis)
  
  col_subset <- seq_len(nrow(basisSet(.data, basis)))
  vals <- .retrieve_mat(.data, basis, .subset = row_subset)
  vals <- vals[, col_subset, drop = FALSE]
  if (is(vals, "LinearEmbeddingMatrix")) {
    vals <- sampleFactors(vals)
  }
  
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


.norm_args_sneezy <- function(.data, basis, neighbor) {
  stopifnot(is(.data, "TourExperiment"))
  stopifnot(
    (is.numeric(basis) | is.character(basis)) & length(basis) == 1
  )
  stopifnot(
    (is.numeric(neighbor) | is.character(neighbor)) & length(neighbor) == 1
  )
}


.set_basis <- function(.data, basis) {
  # change to character for retrieval
  if (is.numeric(basis)) return(basisSetNames(.data)[basis])
  basis
}

.setup_plan <- function(.data, basis) {
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

# the guts for generating a basis set, mostly pillaged
# from tourr, for large basis sets could consider using DelayedArray backend?
.tour_path <- function(.data, tour_path, start, max_bases, step_size) {
  # checks
  stopifnot(is.numeric(step_size), 
            is.numeric(max_bases), 
            is.null(start) || is.matrix(start)
  )
  
  # set up generator
  tour <- tourr::new_tour(.data, tour_path, start)
  # starting projection
  start <- tour(0)$proj
  # dimensions 
  nc <- ncol(.data)
  dims <- c(nc, ncol(start),  max_bases + 1) 
  projs <- array(NA_real_, dims)
  
  i <- 0L 
  while (i < max_bases) {
    
    i <- i + 1
    step <- tour(step_size)
    projs[,,i] <- step$target
    if (step$step < 0) 
      break
    
  }

  empty <- apply(projs, 3, function(x) all(is.na(x)))
  projs <- projs[, , !empty, drop = FALSE]
  projs
}

#' @name generate_bases
#' @rdname generate_bases
#' @export
setMethod("generate_bases",
          signature = "ANY",
          function(.data, .on = NULL, subset = NULL, clamp = FALSE, max_bases = 100, start = NULL, step_size = Inf, .engine = tourr::grand_tour()) {
            stopifnot(inherits(.engine, "tour_path")) 
  
            if (clamp) {
              .data <- .rescale(.data)
            }
            
            .tour_path(.data, 
                       tour_path = .engine, 
                       start = start, 
                       max_bases = max_bases, 
                       step_size = step_size)
          })


#' @name generate_bases
#' @rdname generate_bases
#' @export
setMethod("generate_bases",
          signature = "LinearEmbeddingMatrix",
          function(.data, .on = NULL, subset = NULL, clamp = FALSE, max_bases = 100, start = NULL, step_size = Inf, .engine = tourr::grand_tour()) {
            vals <- sampleFactors(.data)
            if (!is.null(subset)) {
              vals <- vals[, subset, drop = FALSE]
            }
            generate_bases(vals, .on, subset = NULL, clamp, max_bases, start, step_size, .engine)
          })
            
#' @name generate_bases
#' @rdname generate_bases
#' @export
setMethod("generate_bases",
          signature = "TourExperiment",
          function(.data, .on = NULL, subset = NULL, clamp = FALSE, max_bases = 100, start = NULL, step_size = Inf, .engine = tourr::grand_tour()) {
            vals <- .retrieve_mat(.data, .on)
            if (!is.null(subset)) {
              vals <- vals[, subset, drop = FALSE]
            }
            path <- generate_bases(vals, 
                                   .on, 
                                   subset = NULL, 
                                   clamp = clamp, 
                                   max_bases = max_bases, 
                                   start = NULL, 
                                   step_size = step_size, 
                                   .engine = .engine)
            
            basisSet(.data, .on) <- path
            .data
            
          })


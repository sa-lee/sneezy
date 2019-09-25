.rescale <- function(.data) {
  rng <- DelayedArray::colRanges(.data)
  vals <- sweep(.data, 2, rng[,1])
  sweep(vals, 2, rng[,2] - rng[, 1], FUN = "/")  
}


.tour_path <- function(.data, tour_path, start, max_bases, step_size) {
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


# slightly faster version of proj_dist in tourr via tcrossprod
fproj_dist <- function(x, y) {
  sqrt(sum((tcrossprod(x) - tcrossprod(y))^2))
} 


.interpolate <- function(basis_set, angle) {
  
  # dimensions
  nr <- nrow(basis_set)
  nc <- ncol(basis_set)
  nb <- dim(basis_set)[3]
  
  # flatten into a list
  basis_set  <- Map(function(x) x[[1]], apply(basis_set, 3, list))
  
  # compute distance between realised bases
  dists <- vapply(seq.int(2, nb), 
                  function(i) fproj_dist(basis_set[[i-1]], basis_set[[i]]),
                  numeric(1)
  )
  
  # steps are a function of distances between bases and the angle
  # in radians
  steps <- sum(ceiling(dists/angle)) * 2
  new_basis <- vector(length = steps)
  new_basis[1] <- TRUE
  
  projs <- array(NA_real_, c(nr, nc, steps))
  i <- 1
  
  tour <- tourr::new_tour(basis_set[[1]], planned_tour(basis_set))
  
}

#' @export
setMethod("generate_bases",
          signature = "ANY",
          function(.data, .on = NULL, clamp = FALSE, max_bases = 100, start = NULL, step_size = Inf, .engine = tourr::grand_tour()) {
            stopifnot(inherits(.engine, "tour_path")) 
              if (clamp) {
                .data <- .rescale(.data)
              }
              .tour_path(.data, .engine, start, max_bases, step_size)
          })


setMethod("generate_bases",
          signature = "LinearEmbeddingMatrix",
          function(.data, .on = NULL, clamp = FALSE, max_bases = 100, start = NULL, step_size = Inf, .engine = tourr::grand_tour()) {
            vals <- sampleFactors(.data)
            generate_bases(vals, .on, clamp, max_bases, start, step_size, .engine)
          })
            

#' @export
setMethod("generate_bases",
          signature = "TourExperiment",
          function(.data, .on = NULL, clamp = FALSE, max_bases = 100, start = NULL, step_size = Inf, .engine = tourr::grand_tour()) {
            vals <- .retrieve_mat(.data, .on)
            path <- generate_bases(vals, .on, clamp, max_bases, start, step_size, .engine)
            
            basisSet(.data, .on) <- path
            .data
            
          })


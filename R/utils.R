# slightly faster version of proj_dist in tourr via tcrossprod
fproj_dist <- function(x, y) {
  sqrt(sum((tcrossprod(x) - tcrossprod(y))^2))
}

# rescale to have values lie in [0,1]. Unlike the tourr version
# allows for a DelayedArray backend
.rescale <- function(.data) {
  rng <- DelayedArray::colRanges(.data)
  vals <- sweep(.data, 2, rng[,1])
  sweep(vals, 2, rng[,2] - rng[, 1], FUN = "/")  
}

# check if shiny installed
.check_shiny <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Please install shiny", call. = FALSE)
  }
}

# zero pad a vector at even index 
pad_zeros <- function(x) {
  n <- length(x) * 2
  ans <- rep(0L, n)
  inx <- seq.int(2, n*2, by = 2)
  ans[inx] <- x
  ans  
}

flatten_array <- function(projs) Map(function(x) x[[1]], apply(projs, 3, list))

#' @export
setMethod("compute_half_range", 
          "ANY", 
          function(.data)   max(sqrt(rowSums(.data^2)))
)

#' @export
setMethod("compute_half_range", 
          "LinearEmbeddingMatrix",
          function(.data) compute_half_range(sampleFactors(.data))
)

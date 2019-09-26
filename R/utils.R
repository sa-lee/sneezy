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

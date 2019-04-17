#' Fast sphering of data
#' 
#' @param data a numerical matrix 
#' @param k number of principal components to return
#' @importFrom irlba prcomp_irlba
#' 
#' @description Like tourr::sphere_data but gives you control 
#' over number of components returned.
#' 
fsphere_data <- function(data, k) {
  if (!requireNamespace("irlba", quietly = TRUE)) {
    stop("Fast sphering requires the irlba package to be installed.")
  }
  dr <- irlba::prcomp_irlba(data, n = k)
  scale(stats::predict(dr))
}
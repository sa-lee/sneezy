#' Multi-challenge data set:
#' or data that lies in 10 dimensions.
#' 
#' @details 
#' The data has 1000 observations, consisting of five subsets of 200 
#' observations each. The subsets each have different structure in 
#' high dimensional space:
#' 
#' * Subset A: A Gaussian cluster consisting of three sub clusters in 3-dimensions.
#' * Subset B: Overlapping Gaussian clusters in 3-dimensions. 
#'   The number of points is skewed, as the first cluster has twice as many points
#'   as the second.
#' * Subset C: Two well separated Gaussian clusters in 10-dimensions. 
#' * Subset D: Intertwined rings in 3-dimesions.
#' * Subest E:  Four piecwise lines produced from a sampling along a curve
#' in 4 dimensions. Each line segment is parallel to an axis in 4-d. 
#' As the points get closer to the ends of the curve the the sampling noise
#' increases.
#'
#' All subsets are normalised to have mean 0 and variance 1. 
#' 
#' For more detail see the source.   
#' 
#' @format The data
#' * key: The name of subset
#' * index: The row index of each subet
#' * X1-X10: The values of each dimension from 1 to 10   
#' 
#' @source [http://ifs.tuwien.ac.at/dm/dataSets.html](http://ifs.tuwien.ac.at/dm/dataSets.html)
"multi"


#' Sample from a p-dimensional solid sphere
#' 
#' @param n number of samples
#' @param p number of dimensions
#' @param mean,sd passed to `stats::rnorm`
#' 
#' @export
#' @examples 
#' generate_sphere(1000, 10, mean =  5, sd = 2)
generate_sphere <- function(n, p, mean, sd) {
  # hollow
  sphere <- matrix(
    rnorm(n*p, mean = mean, sd = sd),
    ncol = p
  )
  # sweep by l2 norm
  sphere <- t(apply(sphere, 1, function(.x) { .x / sqrt(sum(.x^2)) }))
  # spread accross
  sphere <- sphere * runif(n)^(1/p)
  colnames(sphere) <- seq_len(p)
  sphere
}
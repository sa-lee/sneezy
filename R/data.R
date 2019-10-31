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


#' Parton distribution function sensitivity experiments
#' 
#' @description Data from Wang et al., 2018 to compare embedding approaches to a
#' tour path.
#' 
#' @details Data were obtained from CT14HERA2 parton distribution function
#' fits as used in Laa et al., 2018. There are 28 directions in the parameter
#' space of parton distribution function fit, each point in the variables
#' labelled X1-X56 indicate moving +- 1 standard devation from the 'best' 
#' (maximum likelihood estimate) fit of the function. Each observation has
#' all predictions of the corresponding measurement from an experiment. 
#' 
#' (see table 3 in that paper for more explicit details). 
#' 
#'  The remaining columns are:
#' 
#' * InFit: A flag indicating whether an observation entered the fit of 
#'   CT14HERA2 parton distribution function
#' * Type: First number of ID
#' * ID: contains the identifier of experiment, 1XX/2XX/5XX correpsonds
#' to Deep Inelastic Scattering (DIS) / Vector Boson Production (VBP) / 
#'  Strong Interaction (JET). Every ID points to an experimental paper.   
#' * pt: the per experiment observational id
#' * x,mu: the kinematics of a parton. x is the parton momentum fraction, and
#' mu is the factorisation scale.
#' 
#' @references 
#' Wang, B.-T., Hobbs, T. J., Doyle, S., Gao, J., Hou, T.-J., Nadolsky, P. M., 
#' & Olness, F. I. (2018). PDFSense: Mapping the sensitivity of 
#' hadronic experiments to nucleon structure. 
#' Retrieved from [http://arxiv.org/abs/1808.07470](http://arxiv.org/abs/1808.07470)
#' 
#' Cook, D., Laa, U., & Valencia, G. (2018). 
#' Dynamical projections for the visualization of PDFSense data. 
#' The European Physical Journal C, 78(9), 742. 
#' [https://doi.org/10.1140/epjc/s10052-018-6205-2](https://doi.org/10.1140/epjc/s10052-018-6205-2)
#' 
#' 
#' @source [http://www.physics.smu.edu/botingw/PDFsense_web_histlogy](http://www.physics.smu.edu/botingw/PDFsense_web_histlogy)
"pdfsense"

#' Sample from a p-dimensional solid sphere
#' 
#' @param n number of samples
#' @param p number of dimensions
#' @param mean,sd passed to [stats::rnorm]
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
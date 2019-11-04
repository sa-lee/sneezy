#' Align/rotate one embedding onto another
#' 
#' @param x a numeric matrix of unaligned embedding coordinates
#' @param y a numeric matrix to align `x`` to
#' @param ... other arguments to pass [BiocSingular::runSVD]
#' 
#' @details This is a (naive implementation) of Procuste's analysis,
#' the goal is to find a rotation matrix that most closely aligns the 
#' matrix `x` to the matrix `y` by centering and rescaling. One method of solving this problem
#' is to perform singular value decomposition on 
#' `crossprod(y - colMeans(y), x - colMeans(x))`.
#' 
#' @importFrom BiocSingular runSVD
#' @return A matrix of dimension equal to `x`
#' @name align-embedding
#' @rdname align-embedding
#' @export
align_embedding <- function(x,y, ...) {
  
  R <- rotate_embedding(x, y, ...)
  x %*% R
}

#' @name align-embedding
#' @rdname align-embedding
#' @export
rotate_embedding <- function(x,y, ...) {
  stopifnot(ncol(x) == ncol(y))
  
  # compute scale
  scale <- fproj_dist(y,x)
  
  # center and rescale the embeddings 
  yy <-(y - colMeans(y)) / scale
  xx <- (x - colMeans(x)) / scale
  
  # evaluate covariance, rescaled 
  cov <- crossprod(yy, xx)  
  
  # set up svd
  svd <- BiocSingular::runSVD(cov, ncol(cov), ...)
  
  d <- sign(det(tcrossprod(svd$v, svd$u)))
  if (d <= 0) d <- -1
  
  i <- diag(ncol = ncol(x), nrow = ncol(x))
  i[ncol(x), ncol(x)] <- d
  
  tcrossprod(svd$v %*% i, svd$u)
}
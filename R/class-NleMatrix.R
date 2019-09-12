#' @include class-NleParam.R
 
#' @title NonLinearEmbeddingMatrix class
#' 
#' @description The NonLinearEmbeddingMatrix class is used
#' for storing low-dimensional embeddings from non-linear
#' dimensionality reduction techniques. 
#' 
#' @inheritParams SingleCellExperiment::LinearEmbeddingMatrix
#' @param param An `NonLinearEmbeddingParam` object

#' @details 
#' The `NonLinearEmbeddingMatrix` class inherits
#' from [`SingleCellExperiment::LinearEmbeddingMatrix()`] and is
#' designed to be stored inside a `TourExperiment()` object. It
#' gains an additional slot, storing `NonLinearEmbeddingParam-class`
#' object, allowing us to maintain the arguments used to generate
#' the embedding matrix. This is useful for running diagnostics over
#' the same parameter set.
#'         
#' @importFrom SingleCellExperiment LinearEmbeddingMatrix
#' @seealso [`SingleCellExperiment::LinearEmbeddingMatrix()`]
#' @rdname NonLinearEmbeddingMatrix
#' @export
setClass("NonLinearEmbeddingMatrix", 
         slots = c("param" = "NonLinearEmbeddingParam"),
         contains = "LinearEmbeddingMatrix")


#' @rdname NonLinearEmbeddingMatrix
#' @export
NonLinearEmbeddingMatrix <- function(
  sampleFactors =  matrix(nrow = 0, ncol = 0),
  featureLoadings = matrix(nrow = 0, ncol = 0),
  factorData = NULL,
  metadata = list(),
  param
) {
  lem <- SingleCellExperiment::LinearEmbeddingMatrix(
    sampleFactors, 
    featureLoadings,
    factorData,
    metadata)
  new("NonLinearEmbeddingMatrix", param = param, lem)
} 
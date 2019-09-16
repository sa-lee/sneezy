# Methods for NLEMatrix 



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
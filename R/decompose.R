pca_exact <- BiocSingular::ExactParam
pca_irlba <- BiocSingular::IrlbaParam
pca_random <- BiocSingular::RandomParam


.arg_check_decompose <- function(.data, num_comp, center, scale, .parallel = BiocParallel::SerialParam()) {
  stopifnot(is(.data, "SightSE"))
  stopifnot(is.numeric(num_comp))
  stopifnot((is.logical(center) && length(center) == 1)|| length(center) == ncol(.data))
  stopifnot((is.logical(scale) && length(scale) == 1) || length(scale) == ncol(.data))
  stopifnot(is(.parallel, "BiocParallelParam"))
}

#' Preprocess a SightSE via dimension reduction
#' 
#' @param .data A `SightSE`
#' @param num_comp Number of components to retain
#' @param center Should columns be centered?
#' @param scale Should  columns be scaled to unit variance?
#' @param .parallel A BiocParallel::BPPARAM object
#' @param .method A BiocSingular object 
#' 
#' @export 
setGeneric("decompose", 
           signature = c(".method"),
           function(.data, num_comp, center = TRUE, scale = FALSE, .parallel = BiocParallel::SerialParam(), .method) {
             standardGeneric("decompose")
           })


#' @importClassesFrom BiocSingular BiocSingularParam
#' @importFrom BiocSingular runPCA
#' @importFrom SummarizedExperiment assay
#' @importFrom SingleCellExperiment reducedDim<- LinearEmbeddingMatrix
#' @export
setMethod("decompose", "BiocSingularParam", 
          function(.data, num_comp, center = TRUE, scale = FALSE, .parallel = BiocParallel::SerialParam(), .method) {
            .arg_check_decompose(.data, num_comp, center, scale, .parallel)
            res <- runPCA(assay(.data), 
                          rank = num_comp, 
                          center = center, 
                          scale = scale,
                          BPPARAM = .parallel,
                          BSPARAM = .method
            )
            
            .name <- as.character(substitute(.method))[1]
            reducedDim(.data, .name) <- LinearEmbeddingMatrix(
              sampleFactors = res$rotation,
              featureLoadings = res$x,
              factorData = as(data.frame(sdev = res$sdev), "DataFrame")
            ) 
            .data
          })






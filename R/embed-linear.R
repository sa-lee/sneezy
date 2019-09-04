#' Linear Embedding Drivers
#' 
#' @details Different engines for computing 
#' linear embeddings (mostly PCA at this stage). Usually you
#' won't need to interact with these functions directly.
#' 
#' @return A `BiocSingular::BiocSingularParam()` object. 
#' 
#' @rdname le-backends
#' @importFrom BiocSingular ExactParam IrlbaParam RandomParam
#' @export
pca_exact <- BiocSingular::ExactParam
#' @rdname le-backends
#' @export
pca_irlba <- BiocSingular::IrlbaParam
#' @rdname le-backends
#' @export
pca_random <- BiocSingular::RandomParam


.arg_check_le <- function(.data, num_comp, center, scale, .parallel = BiocParallel::SerialParam()) {
  stopifnot(is(.data, "SightSE"))
  stopifnot(is.numeric(num_comp))
  stopifnot((is.logical(center) && length(center) == 1)|| length(center) == ncol(.data))
  stopifnot((is.logical(scale) && length(scale) == 1) || length(scale) == ncol(.data))
  stopifnot(is(.parallel, "BiocParallelParam"))
}

#' Compute a linear embedding on a `SightSE` object. 
#' 
#' @param .data A `sneezy::SightSE()` object
#' @param num_comp Number of components to retain
#' @param center Should columns be centered? Default = TRUE
#' @param scale Should  columns be scaled to unit variance?
#' @param .parallel A BiocParallel::BPPARAM() object
#' @param .engine A BiocSingular::BiocSingular() object 
#' used to compute the embedding. Deafults to `pca_exact()`  
#' 
#' @export 
setGeneric("embed_linear", 
           signature = c(".engine"),
           function(.data, num_comp, center = TRUE, scale = FALSE, .parallel = BiocParallel::SerialParam(), .engine = pca_exact()) {
             standardGeneric("embed_linear")
           })


#' @importClassesFrom BiocSingular BiocSingularParam
#' @importFrom BiocSingular runPCA
#' @importFrom SummarizedExperiment assay
#' @importFrom SingleCellExperiment reducedDim<- LinearEmbeddingMatrix
#' @export
setMethod("embed_linear", "BiocSingularParam", 
          function(.data, num_comp, center = TRUE, scale = FALSE, .parallel = BiocParallel::SerialParam(), .engine = pca_exact()) {
            .arg_check_le(.data, num_comp, center, scale, .parallel)
            res <- runPCA(assay(.data), 
                          rank = num_comp, 
                          center = center, 
                          scale = scale,
                          BPPARAM = .parallel,
                          BSPARAM = .engine
            )
            
            .name <- as.character(substitute(.engine))[1]
            reducedDim(.data, .name) <- LinearEmbeddingMatrix(
              sampleFactors = res$rotation,
              featureLoadings = res$x,
              factorData = as(data.frame(sdev = res$sdev), "DataFrame")
            ) 
            .data
})

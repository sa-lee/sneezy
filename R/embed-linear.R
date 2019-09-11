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

#' Compute a linear embedding over a `TourExperiment` object. 
#' 
#' @param .data A `TourExperiment()` object
#' @param num_comp Number of components to retain
#' @param .on The named element in `.data` to compute the PCA (default is NULL which
#' is the first assay in `.data`.
#' @param center Should columns be centered? Default = TRUE
#' @param scale Should columns be scaled to unit variance?
#' @param .parallel A `BiocParallel::BPPARAM()`` object, default is to compute in serial.
#' @param .engine How to compute the embedding. Deafults to `pca_exact()`.  
#' 
#' @details This function is a wrapper to `BiocSingular::runPCA()`, with
#' additions for computing on parts of `TourExperiment` object. This function
#' always returns a `TourExperiment` with the `reducedDim` slot updated
#' with `SingleCellExperiment::LinearEmbeddingMatrix` containing the
#' sample factors, loadings, and factor data from the principal components. 
#' 
#' 
#' 
#' @export 
setGeneric("embed_linear", 
           signature = c(".engine"),
           function(.data, num_comp, .on = NULL, center = TRUE, scale = FALSE, .parallel = BiocParallel::SerialParam(), .engine = pca_exact()) {
             standardGeneric("embed_linear")
           })


#' @importClassesFrom BiocSingular BiocSingularParam
#' @importFrom BiocSingular runPCA
#' @importFrom SummarizedExperiment assay
#' @importFrom SingleCellExperiment reducedDim<- LinearEmbeddingMatrix
#' @export
setMethod("embed_linear", "BiocSingularParam", 
          function(.data, num_comp, .on = NULL, center = TRUE, scale = FALSE, .parallel = BiocParallel::SerialParam(), .engine = pca_exact()) {
            .arg_check_le(.data, num_comp, center, scale, .parallel)
            
            val <- .retrieve_mat(.data, .on = NULL)
            
            res <- runPCA(val, 
                          rank = num_comp, 
                          center = center, 
                          scale = scale,
                          BPPARAM = .parallel,
                          BSPARAM = .engine
            )
            
            .name <- as.character(substitute(.engine))[1]
            reducedDim(.data, .name) <- LinearEmbeddingMatrix(
              sampleFactors = res$x,
              featureLoadings = res$rotation,
              factorData = as(data.frame(sdev = res$sdev), "DataFrame")
            ) 
            .data
})

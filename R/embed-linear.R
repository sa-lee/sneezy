#' Linear Embedding Drivers
#' 
#' @inheritParams BiocSingular::ExactParam 
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

#' @inheritParams BiocSingular::IrlbaParam
#' @rdname le-backends
#' @export
pca_irlba <- BiocSingular::IrlbaParam

#' @inheritParams BiocSingular::RandomParam
#' @rdname le-backends
#' @export
pca_random <- BiocSingular::RandomParam


.arg_check_le <- function(.data, num_comp, center, scale, .parallel = BiocParallel::SerialParam()) {
  stopifnot(is(.data, "TourExperiment"))
  stopifnot(is.numeric(num_comp))
  stopifnot((is.logical(center) && length(center) == 1)|| length(center) == ncol(.data))
  stopifnot((is.logical(scale) && length(scale) == 1) || length(scale) == ncol(.data))
  stopifnot(is(.parallel, "BiocParallelParam"))
}


#' @name embed_linear
#' @rdname embed_linear
#' @export
setMethod("embed_linear", 
          "missing", 
          function(.data, num_comp, .on = NULL, center = TRUE, scale = FALSE, .subset = NULL, .parallel = BiocParallel::SerialParam(), .engine) {
            embed_linear(.data, 
                         num_comp, 
                         .on, 
                         center, 
                         scale, 
                         .subset, 
                         .parallel, 
                         .engine = pca_exact())
          })


#' @name embed_linear
#' @rdname embed_linear
#' @importClassesFrom BiocSingular BiocSingularParam
#' @importFrom BiocSingular runPCA
#' @importFrom SummarizedExperiment assay
#' @importFrom SingleCellExperiment reducedDim<- LinearEmbeddingMatrix
#' @export
setMethod("embed_linear", 
          "BiocSingularParam", 
          function(.data, num_comp, .on = NULL, center = TRUE, scale = FALSE, .subset = NULL, .parallel = BiocParallel::SerialParam(), .engine) {
            
            .arg_check_le(.data, num_comp, center, scale, .parallel)
            
            val <- .retrieve_mat(.data, from = .on, .subset = .subset)
            
            res <- runPCA(val, 
                          rank = num_comp, 
                          center = center, 
                          scale = scale,
                          BPPARAM = .parallel,
                          BSPARAM = .engine
            )
            
            # coerce to an LEM object
            lem <- LinearEmbeddingMatrix(
              sampleFactors = res$x,
              featureLoadings = res$rotation,
              factorData = as(data.frame(sdev = res$sdev), "DataFrame")
            )
            colnames(lem) <- colnames(res$x)
            rownames(lem) <- rownames(res$x)
            
            .name <- as.character(substitute(.engine))[1]
            reducedDim(.data, .name) <- lem
            .data
          })

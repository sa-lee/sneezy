# save some typing
.tsne_generator <- function(fun, args) {
  
  function(.data, num_comp, .on, normalize = TRUE, .parallel, .engine) {
    val <- .retrieve_mat(.data, .on)
    
    args <- list(X = val,
                 dims = num_comp,
                 normalize = normalize,
                 pca = FALSE)
    # if the params are overwritten by user then this will update it
    args <- utils::modifyList(args, args(.engine))

    .name <- as.character(substitute(.engine))[1]
    res <- do.call(fun, args)
    
    # todo keep the callback param to rerun tsne
    reducedDim(.data,  .name) <- res$Y
    
    .data
  
  }
  
}

#' Compute a non-linear embedding on a `TourExperiment` object. 
#' 
#' @param .data A `TourExperiment()` object
#' @param num_comp Number of components/dimensions to retain
#' @param .on  Where to compute the non-linear embedding? If NULL, 
#' the first assay is computed.
#' @param normalize Avoid numerical precision issues by centering and scaling the
#'  input data. (Default = TRUE). 
#' @param .parallel Register a parallel backend, only used if computing 
#' nearest-neighbors prior to perfoming a non-linear embedding. 
#' @param .engine A `NonLinearEmbeddingParam`` object used to define the 
#' arguments and algorithm for non-linear embedding. Deafults to `tsne_approx()`
#' 
#' @details This function computes a non-linear embedding over the parameters
#' for a non-linear embedding algorithm and adjusts the reducedDim slot with
#' the embedding coordinates. 
#' 
#' @export
#' 
#' 
#' 
#' @seealso `Rtsne::Rtsne()`
#' @importFrom utils modifyList
#' @importFrom Rtsne Rtsne_neighbors Rtsne
setGeneric(
  "embed_nonlinear",
  signature = ".engine",
  function(.data, num_comp, .on = NULL, normalize = TRUE, .parallel = BiocParallel::SerialParam(), .engine = tsne_approx(30, 12, 0.5)) {
    standardGeneric("embed_nonlinear")
  }
)

setMethod("embed_nonlinear",
          "missing",
          .tsne_generator(Rtsne::Rtsne, function(.engine) nleArgs(.engine))
)

setMethod("embed_nonlinear", 
          "tsneParam", 
          .tsne_generator(Rtsne::Rtsne, function(.engine) nleArgs(.engine) )
)
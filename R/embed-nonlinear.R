#' Non-linear Embedding Drivers
#'
#' @details A virtual class for representing parameter inputs to
#' non-linear embedding algorithms. At the moment we have only created 
#' two concrete classes, `tsneParam` and `tsneNeighborsParam`. The
#' interfaces to created these functions.  
#' 
#' @param perplexity The perplexity paramter for t-SNE
#' @param alpha The exaggeration factor parameter for t-SNE
#' @param theta The speed/accuracy trade-off parameter. 
#' @param ... additional arguments forwarded to other methods
#' 
#' @rdname NonLinearEmbeddingParam-class
#' @export
#' 
#' @seealso `embed_nonlinear()`
#' @examples 
#' 
#' tsne_exact()
#' 
setClass("NonLinearEmbeddingParam", 
         contains = "VIRTUAL",
         slots = c("args" = "list")
)

#' @rdname NonLinearEmbeddingParam-class
#' @export
setGeneric("nleArgs", function(object) standardGeneric("nleArgs"))
#' @rdname NonLinearEmbeddingParam-class
#' @export
setMethod("nleArgs", "NonLinearEmbeddingParam", function(object) object@args)

#' @export
setMethod("show", 
          "NonLinearEmbeddingParam", 
          function(object) { 
            cat(sprintf("class: %s\n", class(object)))
            cat("arguments:\n")
            show(as.data.frame(nleArgs(object)))
          }
)

#' @rdname NonLinearEmbeddingParam-class
#' @export
setClass("tsneParam", contains = "NonLinearEmbeddingParam")

#' @rdname NonLinearEmbeddingParam-class
#' @export
setClass("tnseNeighborsParam", contains = "tsneParam")


#' @rdname NonLinearEmbeddingParam-class
#' @export
tsne_exact <- function(perplexity, alpha, ...) {
  new("tsneParam", 
      args = list(perplexity = perplexity,
                  exaggeration_factor = alpha,
                  theta = 0, 
                  ...)
  )
}

#' @rdname NonLinearEmbeddingParam-class
#' @export
tsne_approx <- function(perplexity, alpha, theta, ...) {
  new("tsneParam", 
      args = list(
        perplexity = perplexity,
        exaggeration_factor = alpha,
        theta = theta,
        ...
      )
  )
}


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
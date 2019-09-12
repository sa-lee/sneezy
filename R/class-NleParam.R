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

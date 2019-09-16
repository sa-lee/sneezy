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

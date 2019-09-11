#' Non-linear embedding drivers

setClass("NonLinearEmbeddingParam", contains = "VIRTUAL")

setClass("tsneParam", 
         contains = "NonLinearEmbeddingParam",
         slots = c("args" = "list"))

setClass("tnseNeighborsParam", 
         contains = "tsneParam")

tsneArgs <- function(object) object@args

tsne_exact <- function(perplexity, alpha, ...) {
  new("tsneParam", 
      args = list(perplexity = perplexity,
                  exaggeration_factor = alpha,
                  theta = 0, 
                  ...)
  )
}

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

.tsne_generator <- function(fun, args) {
  
  function(.data, num_comp, .on, normalize = TRUE, .parallel, .engine) {
    val <- .retrieve_mat(.data, .on)
    
    args <- list(X = val,
                 dims = num_comp,
                 normalize = normalize,
                 pca = FALSE)
    args <- utils::modifyList(args, args(.engine))

    print(args)
    do.call(fun, args)
    
  }
  
}

#' Compute a non-linear embedding on a `SightSE` object. 
#' 
#' @param .data A `sneezy::SightSE()` object
#' @param num_comp Number of components/dimensions to retain
#' @param .on  Where to compute the non-linear embedding?
#' @param center Should columns be centered? Default = TRUE
#' @param scale Should  columns be scaled to unit variance?
#' @param .parallel A BiocParallel::BPPARAM() object
#' @param .engine A BiocSingular::BiocSingular() object 
#' used to compute the embedding. Deafults to `tsne_exact()`
#' 
#' 
#' @importFrom utils modifyList
#' @importFrom Rtsne Rtsne_neighbors
setGeneric(
  "embed_nonlinear",
  signature = ".engine",
  function(.data, num_comp, .on, normalize = TRUE, .parallel = BiocParallel::SerialParam(), .engine) {
    standardGeneric("embed_nonlinear")
  }
)


setMethod("embed_nonlinear", 
          "tsneParam", 
          .tsne_generator(Rtsne::Rtsne, function(.engine) tsneArgs(.engine) )
)






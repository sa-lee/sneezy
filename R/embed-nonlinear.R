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
setGeneric(
  "embed_nonlinear",
  signature = ".engine",
  function(.data, num_comp, .on, center = TRUE, scale = FALSE, .parallel = BiocParallel::SerialParam(), .engine) {
    standardGeneric("embed_nonlinear")
  }
)




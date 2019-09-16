# Methods for NLEMatrix 



#' @rdname NonLinearEmbeddingMatrix
#' @export
NonLinearEmbeddingMatrix <- function(
  sampleFactors =  matrix(nrow = 0, ncol = 0),
  featureLoadings = matrix(nrow = 0, ncol = 0),
  factorData = NULL,
  metadata = list(),
  param
) {
  lem <- SingleCellExperiment::LinearEmbeddingMatrix(
    sampleFactors, 
    featureLoadings,
    factorData,
    metadata)
  new("NonLinearEmbeddingMatrix", param = param, lem)
}



setMethod("view_xy", 
          signature = "matrix", 
          function(.data, .on = NULL, .x, .y, .color, ...) {
            stopifnot(is.null(.on)) 
            x <- .data[, .x]
            y <- .data[, .y]
            
            if (missing(.color)) {
              .color <- I("black")
            } else {
              .color <- .data[, .color]
            }
            
            p <- plotly::plot_ly(type = "scatter", mode = "markers")
            
            
            p <- plotly::add_markers(p, 
                                   x = x, 
                                   y = y, 
                                   color = .color)
            
            p <- plotly::layout(p, dragmode = "lasso", showlegend = FALSE)
            p <- plotly::toWebGL(p)
            p
            
})

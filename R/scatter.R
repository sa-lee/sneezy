#' Simple interactive xy-plots for TourExperiment objects
#' 
#' @param .data A `TourExperiment` object
#' @param .on Where to scatter? The assay name or reducedDim slot name
#' @param .x The x aesthetic (a character or integer)
#' @param .y The y aesthetic (a character or integer)
#' @param .color The colour aesthetic (either a vector or dimensions from `.on`)
#' 
#' @importFrom SummarizedExperiment colData
#' @export
setMethod("view_xy", "TourExperiment",
          function(.data, .on = NULL, .x, .y, .color, ...) {
            vals <- .retrieve_mat(.data, from = .on)
            
            if (!missing(.color)) {
              # try and rextract color 
              if (is.character(.color) && length(.color) == 1) {
                # assume .color is rownames, in which case
                # grab the rows of the assay
                inx <- which(rownames(.data) == .color) 
                if (length(inx)) {
                  .color <- assay(.data, ...)[inx, ]
                } else {
                  # otherwise see if it's in colData
                  inx <- which(names(colData(.data)) == .color)
                  if (length(inx)) {
                    .color <- colData(.data)[[.color]]
                  }
                }
              }
              
            }
            
            view_xy(vals, .x = .x, .y = .y, .color = .color, ...)
          })


#' @export
setMethod("view_xy", 
          signature = "LinearEmbeddingMatrix", 
          function(.data, .on = NULL, .x, .y, .color, ...) {
            if (is.null(.on)) .on <- "sampleFactors"
            vals <- switch(.on,
                           "sampleFactors" = SingleCellExperiment::sampleFactors(.data),
                           "featureLoadings" = SingleCellExperiment::featureLoadings(.data),
                           "factorData" = SingleCellExperiment::factorData(.data))
            
            
            # no color dispatch to matrix method
            if (missing(.color)) {
              return(view_xy(vals, .x = .x, .y = .y, ...))
            }
            
            # forincluding color we assume .color is interpreted as a vector
            tbl <- as(vals[, c(.x, .y), drop = FALSE], "DataFrame")
            .nms <- colnames(tbl)
            stopifnot(length(.color) == nrow(tbl))
            color <- deparse(substitute(.color))
            tbl[[color]] <- .color
            view_xy(tbl, .x = .nms[1], .y = .nms[2], .color = color)
            
          })


#' @export
setMethod("view_xy", 
          signature = "DFrame",
          function(.data, .on = NULL, .x, .y, .color, ...) {
            stopifnot(is.null(.on))
            
            # set up x, y vars
            tbl <- data.frame(x = .data[[.x]], y = .data[[.y]])
            names(tbl) <- c(.x, .y)
            .x_f <- as.formula(paste0("~", .x))
            .y_f <- as.formula(paste0("~",.y))
            if (!missing(.color)) {
              tbl[[.color]] <- .data[[.color]]
            }
            
            p <- plotly::plot_ly(type = "scattergl", mode = "markers", ...)
            
            if (ncol(tbl) == 3L) {
              p <- plotly::add_markers(p, 
                                       data = tbl,
                                       x = .x_f,
                                       y = .y_f,
                                       color = as.formula(paste0("~", .color)),
                                       type = "scattergl")
            } else {
              p <- plotly::add_markers(p,
                                       data = tbl,
                                       x = .x_f,
                                       y = .y_f,
                                       color = I("black"))
            }
            
            plotly::toWebGL(p) 
            
          })

#' @export
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
            
            p <- plotly::plot_ly(type = "scattergl", mode = "markers")
            
            
            p <- plotly::add_markers(p, 
                                     x = x, 
                                     y = y, 
                                     color = .color)
            
            p
            
          })
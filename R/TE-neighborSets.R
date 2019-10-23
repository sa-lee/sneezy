#' @importFrom SingleCellExperiment int_colData int_colData<-
#' @name neighborSets
#' @rdname neighborSets
#' @export
setMethod("neighborSets", 
          "TourExperiment", 
          function(x, withDimnames = TRUE) {
            value <- as(int_colData(x)[[.neighbor_key]], "SimpleList")
            if (withDimnames) {
              for (i in seq_along(value)) {
                rownames(value[[i]]) <- colnames(x)
              }
            }
            value
          })

setReplaceMethod("neighborSets", 
                 "TourExperiment", 
                 function(x, value) {
                   
                   if (length(value) == 0) {
                     .nset <- int_colData(x)[,0]
                   } else {
                     nrows <- vapply(value, nrow, FUN.VALUE = 0L)
                     stopifnot(all(nrows == ncol(x)))
                     .nset <- do.call("DataFrame", lapply(value, I))
                     if (is.null(names(value))) {
                       colnames(.nset) <- paste0(".unnamed", seq_along(value))
                     }
                   }
                   int_colData(x)[[.neighbor_key]] <- .nset
                   x
                 })


# --- neighborSet, neighborSets, neighborSetNames ---
#' @name neighborSets
#' @rdname neighborSets
#' @export
setMethod("neighborSet", 
          c("TourExperiment", "missing"),
          function(x, type, withDimnames = TRUE) {
            if (length(neighborSetNames(x)) == 0) return(nset)
            neighborSet(x, 1, withDimnames)
          }
)

#' @name neighborSets
#' @rdname neighborSets
#' @export
setMethod("neighborSet", 
          c("TourExperiment", "numeric"), 
          function(x, type, withDimnames = TRUE) {
            nset <- int_colData(x)[[.neighbor_key]]
            out <- tryCatch({
              nset[, type]
            },
            error = function(err) {
              stop("invalid subscript 'type' in  neighborSet(<", 
                   class(x), 
                   ">, type=\"numeric\", ...)'\n",
                   conditionMessage(err))
            })
            
            if (withDimnames) {
              rownames(out) <- colnames(x)
            }
            
            out
          }
)

#' @name neighborSets
#' @rdname neighborSets
#' @export
setMethod("neighborSet", 
          c("TourExperiment", "character"), 
          function(x, type, withDimnames = TRUE) {
            nset <- int_colData(x)[[.neighbor_key]]
            out <- tryCatch({
              nset[, type]
            },
            error = function(err) {
              stop("invalid subscript 'type' in 
                   'neighborSet(<", class(x), ">, type=\"character\", ...)'\n",
                   "'", type, "' not in 'neighborSetNames(<", class(x), ">)'")
            })
            if (withDimnames) {
              rownames(out) <- colnames(x)
            }
            out
          }
)

#' @name neighborSets
#' @rdname neighborSets
#' @export
setReplaceMethod("neighborSet", 
                 c("TourExperiment", "missing"),
                 function(x, type, ..., value) {
                   if (length(neighborSetNames(x)) == 0L) {
                     type <- ".unnamed"
                   } else {
                     type <- 1
                   }
                   neighborSet(x, type) <- value
                   x
                 }
)

#' @name neighborSets
#' @rdname neighborSets
#' @export
setReplaceMethod("neighborSet", 
                 c("TourExperiment", "numeric"),
                 function(x, type, ..., value) {
                   
                   internals <- int_colData(x)
                   
                   if (!is.null(value) && !identical(nrow(value), ncol(x))) {
                     stop("invalid value in neighborSet(<",
                          class(x), ">, type = 'numeric') <- value\n",
                          "nrow(value) does not equal ncol(x)")
                   }
                   
                   if (type[1] > ncol(.nset)) {
                     stop("subscript is out of bounds in neighborSet(<",
                          class(x), ">, type = 'numeric')")
                   }
                   
                   internals[[.neighbor_key]][[type]] <- value
                   int_colData(x) <- internals
                   x
                 }
)

#' @name neighborSets
#' @rdname neighborSets
#' @export
setReplaceMethod("neighborSet", 
                 c("TourExperiment", "character"),
                 function(x, type, ..., value) {
                   internals <- int_colData(x)
                   
                   if (!is.null(value) && !identical(nrow(value), ncol(x))) {
                     stop("invalid value in neighborSet(<",
                          class(x), ">, type = 'numeric') <- value\n",
                          "nrow(value) does not equal ncol(x)")
                   }
                   internals[[.neighbor_key]][[type]] <- value
                   int_colData(x) <- internals
                   x
                 }
)



#' @name neighborSets
#' @rdname neighborSets
#' @export
setMethod("neighborSetNames", "TourExperiment", 
          function(x) colnames(int_colData(x)[[.neighbor_key]]))

#' @name neighborSets
#' @rdname neighborSets
#' @export
setReplaceMethod("neighborSetNames", "TourExperiment", 
                 function(x, value) { 
                   colnames(int_colData(x)[[.neighbor_key]]) <- value
                   x
                 })
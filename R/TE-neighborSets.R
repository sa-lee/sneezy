# --- neighborSet, neighborSets, neighborSetNames ---
#' @name neighborSets
#' @rdname neighborSets
#' @export
setMethod("neighborSet", 
          c("TourExperiment", "missing"),
          function(x, type) {
            nset <- x@neighborSets
            if (length(nset) == 0) nset
            neighborSet(x, 1)
          }
)

#' @name neighborSets
#' @rdname neighborSets
#' @export
setMethod("neighborSet", 
          c("TourExperiment", "numeric"), 
          function(x, type) {
            nset <- x@neighborSets
            out <- tryCatch({
              nset[[type]]
            },
            error = function(err) {
              stop("invalid subscript 'type' in  neighborSet(<", 
                   class(x), 
                   ">, type=\"numeric\", ...)'\n",
                   conditionMessage(err))
            })
            out
          }
)

#' @name neighborSets
#' @rdname neighborSets
#' @export
setMethod("neighborSet", 
          c("TourExperiment", "character"), 
          function(x, type) {
            nset <- x@neighborSets
            out <- tryCatch({
              nset[[type]]
            },
            error = function(err) {
              stop("invalid subscript 'type' in 
                   'neighborSet(<", class(x), ">, type=\"character\", ...)'\n",
                   "'", type, "' not in 'neighborSetNames(<", class(x), ">)'")
            })
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
                   nset <- neighborSets(x)
                   if (type[1] > length(nset)) {
                     stop("subscript is out of bounds")
                   }
                   # checks on values
                   neighborSets(x)[[type]] <- value
                   x
                 }
)

#' @name neighborSets
#' @rdname neighborSets
#' @export
setReplaceMethod("neighborSet", 
                 c("TourExperiment", "character"),
                 function(x, type, ..., value) {
                   # checks on values
                   neighborSets(x)[[type]] <- value
                   x
                 }
)

#' @name neighborSets
#' @rdname neighborSets
#' @export
setMethod("neighborSets", "TourExperiment", function(x) x@neighborSets)

#' @name neighborSets
#' @rdname neighborSets
#' @export
setMethod("neighborSetNames", "TourExperiment", 
          function(x) names(neighborSets(x)))

#' @name neighborSets
#' @rdname neighborSets
#' @export
setReplaceMethod("neighborSetNames", "TourExperiment", 
                 function(x, value) { 
                   names(neighborSets(x)) <- value
                   x
                 })
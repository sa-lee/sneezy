# Basis set is stored as an internal metadata element as `data.frame``

#' @name basisSets
#' @rdname basisSets
#' @export
setMethod("basisSets", "TourExperiment", 
          function(x) int_metadata(x)[[.basis_key]])

is_arrayish <- function(x)  is(x, "array") || is(x, "DelayedArray")

#' @name basisSets
#' @rdname basisSets
#' @export
setReplaceMethod("basisSets",
                 "TourExperiment",
                 function(x, value) {
                   if (length(value) == 0L) {
                     .bset <- S4Vectors::SimpleList()
                   } else {
                     are_array <- vapply(value, is_arrayish, logical(1))
                     stopifnot(all(are_array))
                     .bset <- S4Vectors::SimpleList(lapply(value, I))
                     if (is.null(names(value))) {
                       names(.bset) <- paste0(".unnamed", seq_along(value))
                     }
                   }
                    int_metadata(x)[[.basis_key]] <- .bset
                    x
                 })



#' @name basisSets
#' @rdname basisSets
#' @export
setMethod("basisSet", 
          c("TourExperiment", "missing"),
          function(x, type) {
            if (length(basisSetNames(x)) == 0) return(NULL)
            basisSet(x, 1)
          }
)

#' @name basisSets
#' @rdname basisSets
#' @export
setMethod("basisSet", 
          c("TourExperiment", "numeric"), 
          function(x, type) {
            bset <- int_metadata(x)[[.basis_key]]
            out <- tryCatch({
              bset[[type]]
            },
            error = function(err) {
              stop("invalid subscript 'type' in  basisSet(<", 
                   class(x), 
                   ">, type=\"numeric\", ...)'\n",
                   conditionMessage(err))
            })
            out
          }
)

#' @name basisSets
#' @rdname basisSets
#' @export
setMethod("basisSet", 
          c("TourExperiment", "character"), 
          function(x, type) {
            bset <- int_metadata(x)[[.basis_key]]
            out <- tryCatch({
              bset[[type]]
            },
            error = function(err) {
              stop("invalid subscript 'type' in 
                   'basisSet(<", class(x), ">, type=\"character\", ...)'\n",
                   "'", type, "' not in 'basisSetNames(<", class(x), ">)'")
            })
            out
          }
)

#' @name basisSets
#' @rdname basisSets
#' @export
setReplaceMethod("basisSet", 
                 c("TourExperiment", "missing"),
                 function(x, type, ..., value) {
                   if (length(basisSetNames(x)) == 0L) {
                     type <- ".unnamed"
                   } else {
                     type <- 1
                   }
                   basisSet(x, type) <- value
                   x
                 }
)

#' @name basisSets
#' @rdname basisSets
#' @export
setReplaceMethod("basisSet", 
                 c("TourExperiment", "numeric"),
                 function(x, type, ..., value) {
                   bset <- int_metadata(x)[[.basis_key]]
                   if (type[1] > length(bset)) {
                     stop("subscript is out of bounds")
                   }
                   # checks on values
                   stopifnot(is_arrayish(value))
                   bset[[type]] <- value
                   int_metadata(x)[[.basis_key]] <- bset
                   x
                 }
)

#' @name basisSets
#' @rdname basisSets
#' @export
setReplaceMethod("basisSet", 
                 c("TourExperiment", "character"),
                 function(x, type, ..., value) {
                   bset <-  int_metadata(x)[[.basis_key]]
                   # checks on values
                   stopifnot(is_arrayish(value))
                   bset[[type]] <- value
                   int_metadata(x)[[.basis_key]] <- bset
                   x
                 }
)



#' @name basisSets
#' @rdname basisSets
#' @export
setMethod("basisSetNames", "TourExperiment", 
          function(x) names(int_metadata(x)[[.basis_key]]))

#' @name basisSets
#' @rdname basisSets
#' @export
setReplaceMethod("basisSetNames", "TourExperiment", 
                 function(x, value) { 
                   names(int_metadata(x)[[.basis_key]]) <- value
                   x
                 })
#' Access or set the basisSets  of a TourExperiment
#' 
#' @param x an object (a TourExperiment)
#' @param type the name or a number of the list to access
#' @param value a matrix to assign to an element of basisSets
#' 
#' @return a `TourExperiment` object
#' 
#' @name basisSets
#' @rdname basisSets
#' @export
setGeneric("basisSet", function(x, type, ...) standardGeneric("basisSet"))

#' @name basisSets
#' @rdname basisSets
#' @export
setGeneric("basisSet<-", function(x, type, ..., value) standardGeneric("basisSet<-"))

#' @name basisSets
#' @rdname basisSets
#' @export
setGeneric("basisSets", function(x, type, ...) standardGeneric("basisSets"))

#' @name basisSets
#' @rdname basisSets
#' @export
setGeneric("basisSetNames", function(x, type, ...) standardGeneric("basisSetNames"))

#' @name basisSets
#' @rdname basisSets
#' @export
setGeneric("basisSetNames<-", function(x, type, ..., value) standardGeneric("basisSetNames<-"))

#' @name basisSets
#' @rdname basisSets
#' @export
setMethod("basisSet", 
          c("TourExperiment", "missing"),
          function(x, type) {
            bset <- x@basisSets
            if (length(bset) == 0) return(bset)
            basisSet(x, 1)
          }
)

#' @name basisSets
#' @rdname basisSets
#' @export
setMethod("basisSet", 
          c("TourExperiment", "numeric"), 
          function(x, type) {
            bset <- x@basisSets
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
            bset <- x@basisSets
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
                   bset <- basisSets(x)
                   if (type[1] > length(bset)) {
                     stop("subscript is out of bounds")
                   }
                   # checks on values
                   basisSets(x)[[type]] <- value
                   x
                 }
)

#' @name basisSets
#' @rdname basisSets
#' @export
setReplaceMethod("basisSet", 
                 c("TourExperiment", "character"),
                 function(x, type, ..., value) {
                   # checks on values
                   basisSets(x)[[type]] <- value
                   x
                 }
)

#' @name basisSets
#' @rdname basisSets
#' @export
setMethod("basisSets", "TourExperiment", function(x) x@basisSets)


#' @name basisSets
#' @rdname basisSets
#' @export
setMethod("basisSetNames", "TourExperiment", function(x) names(basisSets(x)))

#' @name basisSets
#' @rdname basisSets
#' @export
setReplaceMethod("basisSetNames", "TourExperiment", 
                 function(x, value) { 
                   names(neighborSets(x)) <- value
                   x
                 })
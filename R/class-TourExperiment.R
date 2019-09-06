#' Represent a data.frame or matrix as a `TourExperiment``
#' 
#' @details A `TourExperiment` object inherits from the 
#' `SingleCellExperiment::SingleCellExperiment()` class. However, we feel
#' that is more general/useful than just for biological data sets. 
#' Briefly, it represents a matrix of homegenous types (n rows by p columns) 
#' alongside `SummarizedExperiment::rowData()` which represents data about 
#' the rows and `SummarizedExperiment::colData()` which represents data about the
#' columns. The `TourExperiment` object  adds two additional slots. The first
#' is a slot called `basisSets` is a `SimpleList`, which represents the 
#' anchor bases from computing a tour. The second is a slot called `neighborSets`
#' which is a `SimpleList` that represents nearest 
#' neighbour indexes and distances. 
#' 

#' @param .data object to convert to a `TourExperiment` object
#' @param basisSets a SimpleList object containing tour bases
#' @param neighborSets a SimpleList conatining nearest neighbours
#' @param ... if `.data` is a data.frame the columns to convert to a matrix,
#' every other column not included in `...` will become rowData
#' @param viewAs the type of matrix if `.data` is a data.frame (default is numeric matrix).
#' 
#' @importFrom SingleCellExperiment SingleCellExperiment
#' @importFrom methods setClass setOldClass setGeneric
#' @export
#' 
#' @examples 
#' sphere <- generate_sphere(1000, 10, mean =  5, sd = 2)
#' te_sphere <- TourExperiment(sphere)
#' te_sphere
#' 
#' # convert a data.frame to a TourExperiment object
#' # this allows you to select columns that will form the assay data
#' te_olive <- TourExperiment(tourr::olive, palmitic:eicosenoic)
#' te_olive
#' 
#' @importFrom  S4Vectors SimpleList
#' @name TourExperiment
#' @rdname TourExperiment-class     
setClass("TourExperiment",
         slots = c("basisSets" = "SimpleList", "neighborSets" = "SimpleList"),
         contains = "SingleCellExperiment"
)


.valid_te <- function(object) {
  msg <- character()
  if (length(object@basisSets) != 0) {
    # check all elements in basisSets have rows = to number of variables
    dim_eq <- vapply(object@basisSets, function(.) nrow(.) != ncol(object))
    if (any(dim_eq)) {
      msg <- c(msg, paste("basisSet(s)", 
                          which(dim_eq), 
                          "have mismatched dimensions."))
    }
  }
  
  if (length(object@neighborSets) !=  0) {
    # check all elements in neighborSets have rows = to number of columns
    dim_eq <- vapply(object@neighborSets, function(.) nrow(.) != ncol(object))
    if (any(dim_eq)) {
      msg <- c(msg, paste("neighborSet(s", which(dim_eq),
               "have mismatched dimensions."))
    }
  }
  if (length(msg) != 0) msg 
  else TRUE
}

# --- Validity checker ---
setValidity("TourExperiment", .valid_te)


# --- Constructor ---
.te  <- function(sce, basisSets, neighborSets) {
  new("TourExperiment", sce, basisSets = basisSets, neighborSets = neighborSets)
}

#' @name TourExperiment
#' @rdname TourExperiment-class 
#' @export    
setGeneric("TourExperiment", signature = ".data", function(.data, basisSets = S4Vectors::SimpleList(), neighborSets = S4Vectors::SimpleList(), ...) {
  standardGeneric("TourExperiment")
})

#' @name TourExperiment
#' @rdname TourExperiment-class 
#' @export    
setMethod("TourExperiment", 
          c("SingleCellExperiment"), 
          function(.data, basisSets, neighborSets, ...) {
            .te(.data, basisSets, neighborSets)
          })

#' @name TourExperiment
#' @rdname TourExperiment-class 
#' @export    
setMethod("TourExperiment", 
          c("SummarizedExperiment"), 
          function(.data, basisSets, neighborSets, ...) {
            .te(as(.data, "SingleCellExperiment"), basisSets, neighborSets)
          })

#' @name TourExperiment
#' @rdname TourExperiment-class 
#' @export    
setMethod("TourExperiment", 
          c("matrix"), 
          function(.data, basisSets, neighborSets, ...) {
            sce <- SingleCellExperiment::SingleCellExperiment(list(view = .data))
            .te(sce, basisSets, neighborSets)
          })

#' @name TourExperiment
#' @rdname TourExperiment-class 
#' @export    
setMethod("TourExperiment", 
          c("data.frame"), 
          function(.data,  basisSets, neighborSets, ..., viewAs = "matrix") {
            # select the matrix part
            if (!requireNamespace("dplyr", quietly = TRUE)) {
              stop("Please install dplyr", call. = FALSE)
            }
            
            mat_part <- dplyr::select(.data, ...)
            all_num <- all(vapply(mat_part, is.numeric, logical(1)))
            if(!all_num) {
              stop("selection must only contain numeric variables",
                   call. = FALSE)
            }
            mat_part <- as(mat_part, viewAs)
            
            row_part <- .data[,
                              !(colnames(.data) %in% colnames(mat_part)), 
                              drop = FALSE]
            se <- SingleCellExperiment::SingleCellExperiment(
              assays = list(view = mat_part),
              rowData = row_part
            )
            .te(se, basisSets, neighborSets)
          })


# --- Setters  and Getters ---
setGeneric("neighborSet", 
           function(x, type, ...) standardGeneric("neighborSet"))

setMethod("neighborSet", c("TourExperiment", "missing"),
          function(x, type) {
            nset <- x@neighborSets
            if (length(nset) == 0) nset
            neighborSet(x, 1)
            })

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

setGeneric("neighborSet<-", 
          function(x, type, ..., value) standardGeneric("neighborSet<-"))

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
                 })

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
                 })

setReplaceMethod("neighborSet", 
                 c("TourExperiment", "character"),
                 function(x, type, ..., value) {
                   # checks on values
                   neighborSets(x)[[type]] <- value
                   x
                 }
)

setGeneric("neighborSets", function(x) standardGeneric("neighborSets"))

setMethod("neighborSets", "TourExperiment", function(x) x@neighborSets)

setGeneric("neighborSets<-",
           function(x, value) standardGeneric("neighborSets<-"))

setGeneric("neighborSetNames", 
           function(x) standardGeneric("neighborSetNames"))

setMethod("neighborSetNames", "TourExperiment", 
          function(x) names(neighborSets(x)))


setGeneric("neighborSetNames<-", 
           function(x) standardGeneric("neighborSetNames<-"))




#' @name TourExperiment
#' @rdname TourExperiment-class 
#' @export
setMethod("show", "TourExperiment", function(object) {
  cat(
    callNextMethod(object)
  )
})

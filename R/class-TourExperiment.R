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
#' @param se a `SingleCellExperiment::SingleCellExperiment()` object
#' @param traveller a function of class `tour_path` (usually tourr::grand_tour())
#' @param .data object to convert to a `TourExperiment` object
#' @param ... if `.data` is a data.frame the columns to convert to a matrix,
#' every other column not included in `...` will become rowData
#' @param view_as the type of matrix if `.data` is a data.frame (default is numeric matrix).
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
         slots = c("basisSet" = "SimpleList", "neighborSet" = "SimpleList"),
         contains = "SingleCellExperiment"
)

.te  <- function(sce, basisSet, neighborSet) {
  new("TourExperiment", basisSet = basisSet, neighborSet = neighborSet)
}

#' @name TourExperiment
#' @rdname TourExperiment-class 
#' @export    
setGeneric("TourExperiment", function(.data, basisSet = S4Vectors::SimpleList(), neighborSet = S4Vectors::SimpleList(), ...) {
  standardGeneric("TourExperiment")
})

#' @name TourExperiment
#' @rdname TourExperiment-class 
#' @export    
setMethod("TourExperiment", 
          c("SingleCellExperiment"), 
          function(.data, basisSet, neighborSet, ...) {
            .te(.data, basisSet, neighborSet)
          })

#' @name TourExperiment
#' @rdname TourExperiment-class 
#' @export    
setMethod("TourExperiment", 
          c("SummarizedExperiment"), 
          function(.data, basisSet, neighborSet, ...) {
            .te(as(.data, "SingleCellExperiment"), basisSet, neighborSet)
          })

#' @name TourExperiment
#' @rdname TourExperiment-class 
#' @export    
setMethod("TourExperiment", 
          c("matrix"), 
          function(.data, basisSet, neighborSet, ...) {
            se <- SingleCellExperiment::SingleCellExperiment(list(view = .data))
            .te(se, basisSet, neighborSet)
          })

#' @name TourExperiment
#' @rdname TourExperiment-class 
#' @export    
setMethod("as_TourExperiment", 
          c("data.frame", "tour_path"), 
          function(.data,  basisSet, neighborSet, ..., view_as = "matrix") {
            # select the matrix part
            if (!requireNamespace("dplyr", quietly = TRUE)) {
              stop("Please install dplyr", call. = FALSE)
            }
            
            mat_part <- dplyr::select(.data, ...)
            all_num <- all(vapply(mat_part, is.numeric, logical(1)))
            if(!all_num) {
              stop("selection must only contain numeric variables",
                   .call = FALSE)
            }
            mat_part <- as(mat_part, view_as)
            
            row_part <- .data[,
                              !(colnames(.data) %in% colnames(mat_part)), 
                              drop = FALSE]
            se <- SingleCellExperiment::SingleCellExperiment(
              assays = list(view = mat_part),
              rowData = row_part
            )
            .te(se, basisSet, neighborSet)
          })

#' @name TourExperiment
#' @rdname TourExperiment-class 
#' @export
setMethod("show", "TourExperiment", function(object) {
  cat(
    callNextMethod(object),
    callNextMethod(object@basisSet),
    callNextMethod(object@neighborSet)
  )
})

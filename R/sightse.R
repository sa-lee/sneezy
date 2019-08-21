#' Register tour_path as an S4 class
#' @importFrom methods setOldClass
setOldClass("tour_path") 

#' Represent a data.frame or matrix as a SightSE object.
#' 
#' @details A `SightSE` object inherits from the 
#' `SingleCellExperiment::SingleCellExperiment()` abstraction. However, we feel
#' that is more general/useful than just for biological data sets. 
#' Briefly it represents a matrix of homegenous types (n rows by p columns) 
#' alongside `SummarizedExperiment::rowData()` which represents data about 
#' the rows and `SummarizedExperiment::colData()` which represents data about the
#' columns. The `SightSE` object  adds an additional slot called a `traveller`
#' which is a callback function that implements a tour path, like
#' `tourr::grand_tour()`.  
#' 
#' @param se a `SingleCellExperiment::SingleCellExperiment()` object
#' @param traveller a function of class `tour_path` (usually tourr::grand_tour())
#' @param .data object to convert to a `SightSE` object
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
#' se_sphere <- as_sightse(sphere, traveller = tourr::grand_tour())
#' se_sphere
#' 
#' # convert a data.frame to a SightSE object
#' # this allows you to select columns that will form the assay data
#' se_olive <- as_sightse(tourr::olive, 
#'                        traveller = tourr::guided_tour(tourr::holes()),
#'                        palmitic:eicosenoic)
#' se_olive
#' 
#' @name SightSE
#' @rdname SightSE-class     
setClass("SightSE",
         slots = c("traveller" = "tour_path"),
         contains = "SingleCellExperiment")

#' @name SightSE
#' @rdname SightSE-class     
#' @export
SightSE <- function(se, traveller = tourr::grand_tour()) {
  new("SightSE", traveller = traveller, se)
}

#' @name SightSE
#' @rdname SightSE-class 
#' @export    
setGeneric("as_sightse", function(.data, traveller, ...) {
  standardGeneric("as_sightse")
})

#' @name SightSE
#' @rdname SightSE-class 
#' @export    
setMethod("as_sightse", 
          c("SingleCellExperiment", "tour_path"), 
          function(.data, traveller) {
            SightSE(.data, traveller)
          })

#' @name SightSE
#' @rdname SightSE-class 
#' @export    
setMethod("as_sightse", 
          c("SummarizedExperiment", "tour_path"), 
          function(.data, traveller) {
            SightSE(as(.data, "SingleCellExperiment"),  traveller)
          })

#' @name SightSE
#' @rdname SightSE-class 
#' @export    
setMethod("as_sightse", 
          c("matrix", "tour_path"), 
          function(.data, traveller) {
            se <- SingleCellExperiment::SingleCellExperiment(list(view = .data))
            SightSE(se, traveller)
          })

#' @name SightSE
#' @rdname SightSE-class 
#' @export    
setMethod("as_sightse", 
          c("data.frame", "tour_path"), 
          function(.data, traveller, ..., view_as = "matrix") {
            # select the matrix part
            if (!requireNamespace("dplyr", quietly = TRUE)) {
              stop("Please install dplyr", call. = FALSE)
            }
            
            mat_part <- as(dplyr::select(.data, ...), view_as)
            row_part <- .data[,!(colnames(.data) %in% colnames(mat_part))]
            se <- SingleCellExperiment::SingleCellExperiment(
              assays = list(view = mat_part),
              rowData = row_part
            )
            SightSE(se, traveller)
          })

#' @name SightSE
#' @rdname SightSE-class 
#' @export
setMethod("show", "SightSE", function(object) {
  cat(
    callNextMethod(object),
    print(object@traveller)
  )
})

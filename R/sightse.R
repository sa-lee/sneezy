#' Register tour_path as an S4 methods
methods::setOldClass("tour_path")

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
#' @importFrom SingleCellExperiment SingleCellExperiment
#' @importFrom methods setClass setOldClass setGeneric
#' @export
#' 
#' @name SightSE-class    
setClass("SightSE",
         slots = c("traveller" = "tour_path"),
         contains = "SingleCellExperiment")

# constructor
SightSE <- function(se, traveller = tourr::grand_tour()) {
  new("SightSE", traveller = traveller, se)
}

# interface 
setGeneric("as_sightse", function(.data, traveller = tourr::grand_tour(), ...) {
  standardGeneric("as_sightse")
})

setMethod("as_sightse", c("SingleCellExperiment", "tour_path"), function(.data, traveller) {
  SightSE(.data, traveller)
})

setMethod("as_sightse", c("SummarizedExperiment", "tour_path"), function(.data, traveller) {
  SightSE(as(.data, "SingleCellExperiment"),  traveller)
})

setMethod("as_sightse", c("matrix", "tour_path"), function(.data, traveller) {
  se <- SingleCellExperiment::SingleCellExperiment(list(view = .data))
  SightSE(se, traveller)
})

setMethod("as_sightse", c("data.frame", "tour_path"), function(.data, traveller, ...) {
  # select the matrix part
  mat_part <- dplyr::select(.data, ...)
  row_part <- dplyr::select(.data, -rlang::enquos(...))
  se <- SingleCellExperiment::SingleCellExperiment(
    assays = list(view = mat_part),
    rowData = row_part
  )
  SightSE(se, traveller)
})

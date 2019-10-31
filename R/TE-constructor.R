#' @param object a `TourExperiment` object
#' @name TourExperiment-class
#' @rdname TourExperiment-class 
#' @export
setMethod("show", "TourExperiment", function(object) {
  .bs_name <- basisSetNames(object)
  .ns_name <- neighborSetNames(object)
  .bn <- length(.bs_name)
  .nn <- length(.ns_name)
   
  if (.bn == 0) .bs_name <- ""
  if (.nn == 0) .ns_name <- ""
 
  cat(callNextMethod(object))
  cat(sprintf("neighborSetNames(%d): %s\n", .nn, .ns_name))
  cat(sprintf("basisSetNames(%d): %s\n", .bn, .bs_name))

})

.valid_te <- function(object) {
  msg <- character()
  if (length(msg) != 0) msg 
  else TRUE
}

# --- Validity checker ---
setValidity("TourExperiment", .valid_te)

# --- Constructor ---
.te  <- function(sce, basisSets, neighborSets) {
  
  te <- new("TourExperiment", sce)
  neighborSets(te) <- neighborSets
  basisSets(te) <- basisSets
  te
}

#' @name TourExperiment
#' @rdname TourExperiment-class 
#' @export    
setMethod("TourExperiment", 
          c("SingleCellExperiment"), 
          function(.data, basisSets = SimpleList(), neighborSets = SimpleList()) {
            .te(.data, basisSets, neighborSets)
          })

#' @name TourExperiment
#' @rdname TourExperiment-class 
#' @export    
setMethod("TourExperiment", 
          c("SummarizedExperiment"), 
          function(.data, basisSets = SimpleList(), neighborSets = SimpleList()) {
            .te(as(.data, "SingleCellExperiment"), basisSets, neighborSets)
          })

#' @name TourExperiment
#' @rdname TourExperiment-class 
#' @export    
setMethod("TourExperiment", 
          c("matrix"), 
          function(.data, ..., assayName = "view", basisSets = SimpleList(), neighborSets = SimpleList()) {
            lst <- list()
            lst[[assayName]] <- .data
            sce <- SingleCellExperiment::SingleCellExperiment(lst)
            .te(sce, basisSets, neighborSets)
          })

#' @name TourExperiment
#' @rdname TourExperiment-class 
#' @export    
setMethod("TourExperiment", 
          c("data.frame"), 
          function(.data, ...,  assayName = "view", assayType = "matrix", basisSets = SimpleList(), neighborSets = SimpleList()) {
            # select the matrix part
            if (!requireNamespace("dplyr", quietly = TRUE)) {
              stop("Please install dplyr", call. = FALSE)
            }
            
            mat_part <- dplyr::select(.data, ...)
            drop_cols <- names(mat_part)
            all_num <- all(vapply(mat_part, is.numeric, logical(1)))
            if(!all_num) {
              stop("selection must only contain numeric variables",
                   call. = FALSE)
            }
            mat_part <- as(mat_part, assayType)
            
            col_part <- .data[,
                              !(names(.data) %in% drop_cols), 
                              drop = FALSE]
            # reorient, and rownames are now variable identfiers
            mat_part <- list(t(mat_part))
            names(mat_part) <- assayName

            se <- SingleCellExperiment::SingleCellExperiment(
              assays = mat_part,
              colData = col_part
            )
            .te(se, basisSets, neighborSets)
          })
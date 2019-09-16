#' @name TourExperiment
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
  if (length(object@basisSets) != 0) {
    
    if (length(names(object@basisSets)) == 0) {
      msg <- c(msg, "basisSets must be a named list.")
    }
    
    # # check all elements in basisSets have rows = to number of variables
    # dim_eq <- vapply(object@basisSets, function(.) nrow(.) != ncol(object))
    # if (any(dim_eq)) {
    #   msg <- c(msg, paste("basisSet(s)", 
    #                       which(dim_eq), 
    #                       "have mismatched dimensions."))
    # }
  }
  
  if (length(object@neighborSets) !=  0) {
    
    if (length(names(object@neighborSets)) == 0) {
      msg <- c(msg, "neighborSets must be a named list.")
    }
    # # check all elements in neighborSets have rows = to number of columns
    # dim_eq <- vapply(object@neighborSets, function(.) nrow(.) != ncol(object))
    # if (any(dim_eq)) {
    #   msg <- c(msg, paste("neighborSet(s)", which(dim_eq),
    #            "have mismatched dimensions."))
    # }
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
          function(.data, basisSets = S4Vectors::SimpleList(), neighborSets = S4Vectors::SimpleList(), ...) {
            .te(as(.data, "SingleCellExperiment"), basisSets, neighborSets)
          })

#' @name TourExperiment
#' @rdname TourExperiment-class 
#' @export    
setMethod("TourExperiment", 
          c("matrix"), 
          function(.data, basisSets = S4Vectors::SimpleList(), neighborSets = S4Vectors::SimpleList(), ...) {
            sce <- SingleCellExperiment::SingleCellExperiment(list(view = .data))
            .te(sce, basisSets, neighborSets)
          })

#' @name TourExperiment
#' @rdname TourExperiment-class 
#' @export    
setMethod("TourExperiment", 
          c("data.frame"), 
          function(.data,  basisSets = S4Vectors::SimpleList(), neighborSets = S4Vectors::SimpleList(), ..., viewAs = "matrix") {
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
            mat_part <- as(mat_part, viewAs)
            
            col_part <- .data[,
                              !(names(.data) %in% drop_cols), 
                              drop = FALSE]
            # reorient, and rownames are now variable identfiers
            mat_part <- t(mat_part)

            se <- SingleCellExperiment::SingleCellExperiment(
              assays = list(view = mat_part),
              colData = col_part
            )
            .te(se, basisSets, neighborSets)
          })
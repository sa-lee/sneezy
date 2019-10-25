# internal functions
.retrieve_mat <- function(.data, from = NULL, .subset = NULL) {
  
  if (is.null(from)) {
    res <- t(assay(.data))
  } else {
    # check slots
    # idea is to get named assay unless there's a reducedDim slot
    # if there isn't then throw an error
    a_selector <- intersect(from, SummarizedExperiment::assayNames(.data))
    if (length(a_selector) == 0) {
      rd_selector <- intersect(from, SingleCellExperiment::reducedDimNames(.data))
      if (length(rd_selector) == 0) {
        stop(paste("`from`:", from, "is not available in object."))
      }
      # subset is ignored if returning a reducedDim slot
      return(SingleCellExperiment::reducedDim(.data, rd_selector))
    } else {
      
      res <- t(SummarizedExperiment::assay(.data, a_selector))
    }
  }
  
  if (!is.null(.subset)) {
    return(res[, .subset])
  }
  res
}


# slightly faster version of proj_dist in tourr via tcrossprod
fproj_dist <- function(x, y) {
  sqrt(sum((tcrossprod(x) - tcrossprod(y))^2))
}

# rescale to have values lie in [0,1]. Unlike the tourr version
# allows for a DelayedArray backend
.rescale <- function(.data) {
  rng <- DelayedArray::colRanges(.data)
  vals <- sweep(.data, 2, rng[,1])
  sweep(vals, 2, rng[,2] - rng[, 1], FUN = "/")  
}

# check if shiny installed
.check_shiny <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Please install shiny", call. = FALSE)
  }
}



flatten_array <- function(projs) Map(function(x) x[[1]], apply(projs, 3, list))

#' @export
setMethod("compute_half_range", 
          "ANY", 
          function(.data)   max(sqrt(rowSums(.data^2)))
)

#' @export
setMethod("compute_half_range", 
          "LinearEmbeddingMatrix",
          function(.data) compute_half_range(sampleFactors(.data))
)

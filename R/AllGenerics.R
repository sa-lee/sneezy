# AllGenerics

# --- TourExperiment generics ---

#' @name TourExperiment
#' @rdname TourExperiment-class 
#' @export    
setGeneric("TourExperiment", 
           signature = ".data", 
           function(.data, basisSets = S4Vectors::SimpleList(), neighborSets = S4Vectors::SimpleList(), ...) {
             standardGeneric("TourExperiment")
           })

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
#' @importFrom SingleCellExperiment int_metadata int_metadata<-
#' @export
setGeneric("basisSet", function(x, type, ...) standardGeneric("basisSet"))

#' @name basisSets
#' @rdname basisSets
#' @export
setGeneric("basisSet<-", function(x, type, ..., value) standardGeneric("basisSet<-"))

#' @name basisSets
#' @rdname basisSets
#' @export
setGeneric("basisSets", function(x, ...) standardGeneric("basisSets"))

#' @name basisSets
#' @rdname basisSets
#' @export
setGeneric("basisSets<-", function(x, ..., value) standardGeneric("basisSets<-"))

#' @name basisSets
#' @rdname basisSets
#' @export
setGeneric("basisSetNames", function(x, type, ...) standardGeneric("basisSetNames"))

#' @name basisSets
#' @rdname basisSets
#' @export
setGeneric("basisSetNames<-", function(x, type, ..., value) standardGeneric("basisSetNames<-"))

#' Access or set the neighborSets of a TourExperiment
#' 
#' @param x an object (a TourExperiment)
#' @param type the name or a number of the list to access
#' @param value a matrix to assign to an element of basisSets
#' 
#' @return a `TourExperiment` object
#' 
#' @name neighborSets
#' @rdname neighborSets
#' @export
setGeneric("neighborSet", function(x, type, ...) standardGeneric("neighborSet"))

#' @name neighborSets
#' @rdname neighborSets
#' @export
setGeneric("neighborSet<-", function(x, type, ..., value) standardGeneric("neighborSet<-"))

#' @name neighborSets
#' @rdname neighborSets
#' @export
setGeneric("neighborSets", function(x, ...) standardGeneric("neighborSets"))

#' @name neighborSets
#' @rdname neighborSets
#' @export
setGeneric("neighborSets<-", function(x, type, ...) standardGeneric("neighborSets<-"))

#' @name neighborSets
#' @rdname neighborSets
#' @export
setGeneric("neighborSetNames", function(x, type, ...) standardGeneric("neighborSetNames"))

#' @name neighborSets
#' @rdname neighborSets
#' @export
setGeneric("neighborSetNames<-", function(x, type, ..., value) standardGeneric("neighborSetNames<-"))


# --- NleParam ---
#' @rdname NonLinearEmbeddingParam-class
#' @export
setGeneric("nleArgs", function(object) standardGeneric("nleArgs"))
#' @rdname NonLinearEmbeddingParam-class
#' @export
setMethod("nleArgs", "NonLinearEmbeddingParam", function(object) object@args)



# --- Embedding ---
#' Compute a linear embedding over a `TourExperiment` object. 
#' 
#' @param .data A `TourExperiment()` object
#' @param num_comp Number of components to retain
#' @param .on The named element in `.data` to compute the PCA (default is NULL which
#' is the first assay in `.data`.
#' @param center Should columns be centered? Default = TRUE
#' @param scale Should columns be scaled to unit variance?
#' @param .subset Restrict linear embedding to run on a subset of rows. (Default = NULL).
#' @param .parallel A `BiocParallel::BPPARAM()`` object, default is to compute in serial.
#' @param .engine How to compute the embedding. If missing, defaults to `pca_exact()`.  
#' 
#' @details This function is a wrapper to `BiocSingular::runPCA()`, with
#' additions for computing on parts of `TourExperiment` object. This function
#' always returns a `TourExperiment` with the `reducedDim` slot updated
#' with `SingleCellExperiment::LinearEmbeddingMatrix` containing the
#' sample factors, loadings, and factor data from the principal components. 
#' 
#' 
#' 
#' @export 
setGeneric(
  "embed_linear", 
  signature = ".engine",
  function(.data, num_comp, .on = NULL, center = TRUE, scale = FALSE, .subset = NULL, .parallel = BiocParallel::SerialParam(), .engine) {
    standardGeneric("embed_linear")
  }
)



#' Compute a non-linear embedding on a `TourExperiment` object. 
#' 
#' @param .data A `TourExperiment()` object
#' @param num_comp Number of components/dimensions to retain
#' @param .on  Where to compute the non-linear embedding? If NULL, 
#' the first assay is computed.
#' @param normalize Avoid numerical precision issues by centering and scaling the
#'  input data. (Default = TRUE). 
#' @param .parallel Register a parallel backend, only used if computing 
#' nearest-neighbors prior to perfoming a non-linear embedding. 
#' @param .engine A `NonLinearEmbeddingParam`` object used to define the 
#' arguments and algorithm for non-linear embedding. Deafults to `tsne_approx()`
#' 
#' @details This function computes a non-linear embedding over the parameters
#' for a non-linear embedding algorithm and adjusts the reducedDim slot with
#' the embedding coordinates. 
#' 
#' @export
#' 
#' 
#' 
#' @seealso `Rtsne::Rtsne()`
#' @importFrom utils modifyList
#' @importFrom Rtsne Rtsne_neighbors Rtsne
#' @export
setGeneric(
  "embed_nonlinear",
  signature = ".engine",
  function(.data, num_comp, .on = NULL, normalize = TRUE, .parallel = BiocParallel::SerialParam(), .engine = tsne_approx(30, 12, 0.5)) {
    standardGeneric("embed_nonlinear")
  }
)

# --- Neighbors ---


# --- Bases ---
#' Generate a new basis set via a tour
#' 
#' @param .data a TourExperiment object or matrix-like object 
#' @param .on Which part of `.data` to tour
#' @param clamp Should the columns of toured data be clamped to lie in [0,1]?
#' @param max_bases Maximum number of bases to generate
#' @param start Optional starting projection for tour
#' @param step_size Distance between each step, set to `Inf` which forces new basis generation
#' @param .engine A tour path generator (defaults to [tourr::grand_tour()])
#' @export
setGeneric("generate_bases",
           signature = ".data",
           function(.data, .on = NULL, clamp = FALSE, max_bases = 100, start = NULL, step_size = Inf, .engine = tourr::grand_tour()) {
             standardGeneric("generate_bases")
           })
             

# --- Visualisation ---
#' @export
setGeneric("view_xy", 
           signature = ".data",
           function(.data, .on = NULL, .x, .y, .color, ...) {
             standardGeneric("view_xy")
           })




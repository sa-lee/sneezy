# AllClasses.R
#' Represent a data.frame or matrix or assay-like object as a `TourExperiment`
#' 
#' @details A `TourExperiment` object inherits from the 
#' `SingleCellExperiment::SingleCellExperiment()` class. However, we feel
#' that is more general/useful than just for biological data sets. 
#' Briefly, it represents a matrix of homegenous types (n rows 'features' by 
#' p columns 'samples'). Each 'feature' is aligned to 
#' `SummarizedExperiment::rowData()` slot which represents data about 
#' the rows and `SummarizedExperiment::colData()` which represents data about the
#' samples. The `TourExperiment` object  includes two additional slots. The first
#' is a slot called `basisSets` is a `SimpleList`, which represents the 
#' anchor bases from computing a tour. The second is a slot called `neighborSets`
#' which is a `SimpleList` that represents nearest 
#' neighbour indexes and distances. 
#' 

#' @param .data object to convert to a `TourExperiment` object
#' @param basisSets a SimpleList object containing tour bases
#' @param neighborSets a SimpleList conatining nearest neighbours
#' @param ... if `.data` is a data.frame the columns to convert to a matrix,
#' every other column not included in `...` will become colData, the names
#' of the columns selected with `...` will be become row names in the resulting
#' object.
#' @param viewAs the type of matrix if `.data` is a data.frame (default is numeric matrix).
#' 
#' @importFrom SingleCellExperiment SingleCellExperiment
#' @importFrom methods setClass setOldClass setGeneric
#' 
#' @seealso `basisSets()`, `neighborSets()`
#' @export
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
setClass("TourExperiment", contains = "SingleCellExperiment")

# internal fields
.neighbor_key <- "neighborSets"
.basis_key <- "basisSets"

#' Non-linear Embedding Drivers
#'
#' @details A virtual class for representing parameter inputs to
#' non-linear embedding algorithms. At the moment we have only created 
#' two concrete classes, `tsneParam` and `tsneNeighborsParam`. The
#' interfaces to created these functions.  
#' 
#' @param perplexity The perplexity paramter for t-SNE
#' @param alpha The exaggeration factor parameter for t-SNE
#' @param theta The speed/accuracy trade-off parameter. 
#' @param ... additional arguments forwarded to other methods
#' 
#' @rdname NonLinearEmbeddingParam-class
#' @export
#' 
#' @seealso `embed_nonlinear()`
#' @examples 
#' 
#' tsne_exact()
#' 
setClass("NonLinearEmbeddingParam", 
         contains = "VIRTUAL",
         slots = c("args" = "list")
)


#' @rdname NonLinearEmbeddingParam-class
#' @export
setClass("tsneParam", contains = "NonLinearEmbeddingParam")

#' @rdname NonLinearEmbeddingParam-class
#' @export
setClass("tnseNeighborsParam", contains = "tsneParam")



#' @title NonLinearEmbeddingMatrix class
#' 
#' @description The NonLinearEmbeddingMatrix class is used
#' for storing low-dimensional embeddings from non-linear
#' dimensionality reduction techniques. 
#' 
#' @inheritParams SingleCellExperiment::LinearEmbeddingMatrix
#' @param param An `NonLinearEmbeddingParam` object

#' @details 
#' The `NonLinearEmbeddingMatrix` class inherits
#' from [`SingleCellExperiment::LinearEmbeddingMatrix()`] and is
#' designed to be stored inside a `TourExperiment()` object. It
#' gains an additional slot, storing `NonLinearEmbeddingParam-class`
#' object, allowing us to maintain the arguments used to generate
#' the embedding matrix. This is useful for running diagnostics over
#' the same parameter set.
#'         
#' @importFrom SingleCellExperiment LinearEmbeddingMatrix
#' @seealso [`SingleCellExperiment::LinearEmbeddingMatrix()`]
#' @rdname NonLinearEmbeddingMatrix
#' @export
setClass("NonLinearEmbeddingMatrix", 
         slots = c("param" = "NonLinearEmbeddingParam"),
         contains = "LinearEmbeddingMatrix")
# save some typing
.tsne_generator <- function(fun, args) {
  
  function(.data, num_comp, .on, normalize = TRUE, .parallel, .engine) {
    val <- .retrieve_mat(.data, .on)
    
    args <- list(X = val,
                 dims = num_comp,
                 normalize = normalize,
                 pca = FALSE)
    # if the params are overwritten by user then this will update it
    args <- utils::modifyList(args, args(.engine))

    .name <- as.character(substitute(.engine))[1]
    res <- do.call(fun, args)
    
    sf <- res$Y
    fl <- matrix(ncol = ncol(sf))
    nlem <- NonLinearEmbeddingMatrix(sampleFactors = sf,
                                     featureLoadings = fl,
                                     param = .engine)
    
    colnames(nlem) <- paste0("Dim", seq_len(ncol(sf)))
    
    reducedDim(.data,  .name) <- nlem
    
    .data
  
  }
  
}

setMethod("embed_nonlinear",
          "missing",
          .tsne_generator(Rtsne::Rtsne, function(.engine) nleArgs(.engine))
)

setMethod("embed_nonlinear", 
          "tsneParam", 
          .tsne_generator(Rtsne::Rtsne, function(.engine) nleArgs(.engine) )
)
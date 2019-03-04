#' Compute Delany triangulation in t-SNE space
#' 
#' @param coords output from [Rtsne::Rtsne()]
#' 
#' @export
get_triangles <- function(coords) {
  tri <- geometry::delaunayn(coords$Y)
  rbind(
    tri[, c(1,2)],
    tri[, c(2,3)],
    tri[, c(3,1)]
  )
}


#' @export
add_triangles <- function(coords) {
  
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  
  tri <- get_triangles(coords)
  Y_df <- as.data.frame(coords$Y)
  mesh <- data.frame(x = Y_df[tri[,1], 1],
                     y = Y_df[tri[, 1], 2],
                     xend = Y_df[tri[,2], 1],
                     yend = Y_df[tri[,2], 2])
  
  ggplot2::geom_segment(data = mesh, 
                        ggplot2::aes(x = x, xend = xend, y = y, yend = yend))
}
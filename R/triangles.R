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
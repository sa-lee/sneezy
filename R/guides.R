student <- function(df = 1) {
  function(mat) {
    n <- nrow(mat)
    d <- ncol(mat)
    num <- 1 + sum(rowSums(mat^2) / df) / n 
    1 - num^(-0.5*(df+d))
  }
}
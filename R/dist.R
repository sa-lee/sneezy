
compute_flat_dist <- function(data, coords) {
  data.frame(
    original = as.numeric(stats::dist(Rtsne::normalize_input(data))),
    embedding = as.numeric(stats::dist(Rtsne::normalize_input(coords$Y)))
  )
}

sneezy_shep <- function(data, coords) {
  
  distances <- data.frame(D = as.numeric(stats::dist(data)),
                          d = as.numeric(stats::dist(coords$Y)))
  
  ggplot2::ggplot(data = distances, ggplot2::aes(x = d, y = D)) +
    ggplot2::geom_point() +
    ggplot2::labs(x = "Embedding distance", y = "Original distance")
  
}
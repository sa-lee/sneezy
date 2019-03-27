# everything here should produce it's own valid specs
spec_tour <- function(half_range) {
  domain <- c(-half_range, half_range)
  
  tour_layer <- list(
    mark = list(type = "circle", clip = TRUE),
    encoding = list(
      x = list(field = "V1", type = "quantitative", 
               scale = list(domain = domain),
               axis = list(title = NULL, 
                           grid = FALSE, 
                           ticks = FALSE,
                           labels = FALSE)
      ),
      y = list(field = "V2", type = "quantitative",
               scale = list(domain = domain),
               axis = list(title = NULL, 
                           grid = FALSE, 
                           ticks = FALSE,
                           labels = FALSE)
      ),
      color = list(
        condition = list(
          selection = list(`or` = list("variableBrush", "embeddingBrush")),
          value = "black"
        ),
        value = "gray")
    )
  )
  
  tour_layer
}



spec_projection <- function(coords) {
  half_range <- max(sqrt(rowSums(coords$Y^2)))
  domain <- c(-half_range, half_range)
  nl_layer <- list(
    title = paste("t-SNE with perplexity", coords$perplexity),
    mark = list(type = "circle", clip = TRUE),
    selection = list(embeddingBrush = list(type = "interval")),
    encoding = list(
      
      x = list(field = "tsne_x", type = "quantitative", 
               scale = list(domain = domain),
               axis = list(title = NULL, 
                           grid = FALSE, 
                           ticks = FALSE,
                           labels = FALSE)
      ),
      y = list(field = "tsne_y", type = "quantitative",
               scale = list(domain = domain),
               axis = list(title = NULL, 
                           grid = FALSE, 
                           ticks = FALSE,
                           labels = FALSE)
      ),
      color = list(
        condition = list(
          selection = "embeddingBrush",
          value = "black"
        ),
        value = "gray")
    )
  )
  nl_layer
}


spec_axes <- function(half_range) {
  domain <- c(-half_range, half_range)
  axes_layer <- list(
    `$schema` = vegawidget::vega_schema(),
    data = list(name = "rotations"),
    mark = list(type = "line", clip = TRUE),
    encoding = list(
      x = list(field = "x", type = "quantitative", 
               scale = list(domain = domain),
               axis = list(title = NULL, 
                           grid = FALSE, 
                           ticks = FALSE,
                           labels = FALSE)
      ),
      y = list(field = "y", type = "quantitative",
               scale = list(domain = domain),
               axis = list(title = NULL, 
                           grid = FALSE, 
                           ticks = FALSE,
                           labels = FALSE)
      ),
      order = list(field = "group", type = "nominal"),
      color = list(value = "black")
    )
  )
  vegawidget::as_vegaspec(axes_layer)
}


spec_shep <- function(dist) {
  domain <- c(0, max(dist) + 0.1)
  shep_layer <- list(
    `$schema` = vegawidget::vega_schema(),
    data = list(name = "shep", values = dist),
    mark = list(type = "circle", clip = TRUE),
    encoding = list(
      x = list(field = "original", type = "quantitative",
               scale = list(domain = domain)),
      y = list(field = "embedding", type = "quantitative",
               scale = list(domain = domain)),
      color = list(value = "black"),
      opacity = list(value = 1 / 10)
    )
  )
  vegawidget::as_vegaspec(shep_layer)
}


spec_dot <- function(vars) {
  dot <- list(
    height = 300,
    # data = list(name = "source", values = tour_data),
    `repeat` = list(row = vars),
    spec = list(
      selection = list("variableBrush" = list(type = "interval", resolve = "global")),
      mark = list(type = "tick"),
      encoding = list(
        x = list(field = list(`repeat` = "row"), type = "quantitative"),
        fill = list(
          condition = list(
            selection = list(`or` = list("variableBrush", "embeddingBrush")),
            value = "black"
          ),
          value = "grey")
      )
    )
  )
  dot
}



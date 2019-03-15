spec_tour <- function(data, half_range) {
  domain <- c(-half_range, half_range)
  
  tour_layer <- list(
    data = list(name = "projections", values = data),
    transform = list(
      list(filter =list(selection = "path"))
    ),
    selection = list(
      path = list(
        type = "single",
        fields = list("key"),
        bind = list("key" = 
                      list(input = "range", 
                           min = min(data$key), 
                           max = max(data$key), 
                           step = 1)
                    )
        )
    ),
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
      color = list(value = "black")
    )
  )

  tour_layer
}

spec_projection <- function(coords) {
  half_range <- max(sqrt(rowSums(coords$Y^2)))
  values <- data.frame(x = coords$Y[,1], y = coords$Y[,2])
  domain <- c(-half_range, half_range)
  nl_layer <- list(
    data = list(name = "nl", values = values),
    mark = list(type = "circle", clip = TRUE),
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
      color = list(value = "black")
    )
  )
  nl_layer
}

setup_frames <- function(data) {
  history <- basic_tour_path(data)
  tour_data <- tourr::rescale(data)
  tour_data <- scale(tour_data, center = TRUE, scale = FALSE)
  half_range  <- max(sqrt(rowSums(tour_data^2)))
  projections <- apply(history, 2, function(.) `%*%`(tour_data, .) )
  projections <- as.data.frame(projections)
  projections$key <- rep(seq_len(100), each = nrow(tour_data))
  projections$index <- rep(seq_len(nrow(tour_data)), 100)
  list(data = projections, half_range = half_range)
}

sneezy <- function(data, coords, dim_chart) {
  input <- setup_frames(data)
  panel_tour <- spec_tour(input$data, input$half_range)
  panel_tsne <- spec_projection(coords)
  spec <- list(
    `$schema` = vegawidget::vega_schema(),
    width = 2*dim_chart,
    height = dim_chart,
    hconcat = list(panel_tour, panel_tsne)
  )
  vegawidget::as_vegaspec(spec)
}

spec_tour <- function(dim_chart, half_range) {
  domain <- c(-half_range, half_range)
  vegawidget::as_vegaspec(
    list(
      `$schema` = vegawidget::vega_schema(),
      width = dim_chart,
      height = dim_chart,
      data = list(name = "projection"),
      mark = list(type = "circle"),
      encoding = list(
        x = list(
          field = "V1",
          type = "quantitative",
          scale = list(domain = domain),
          axis = NULL
        ),
        y = list(
          field = "V2",
          type = "quantitative",
          scale = list(domain = domain),
          axis = NULL

        )
      )
    )
  )
}

ui_tour <- function(history) {
  d <- dim(history)
  
  ui_slider <-
    shiny::sliderInput(
      "slider",
      label = "basis",
      min = 1L,
      max = d[3],
      step = 1L,
      value = 1L,
      sep = "",
      animate = shiny::animationOptions(interval = 500, loop = TRUE)
    )
  
  shiny::fluidPage(
    shiny::titlePanel("Tourring"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        ui_slider
      ),
      shiny::mainPanel(
        vegawidget::vegawidgetOutput("chart")
      )
    )
  )
}

shiny_tour <- function(data, tsne_coords, .subset) {
  history <- basic_tour_path(data)
  tour_data <- tourr::rescale(data)
  tour_data <- scale(tour_data, center = TRUE, scale = FALSE)
  half_range  <- max(sqrt(rowSums(tour_data^2)))
  spec <- spec_tour(400, half_range)
  server <- function(input, output) {
    # reactives
    
    current_projection <- shiny::reactive({
      source <- as.data.frame(tour_data %*% history[,,input$slider] / half_range)
      source$group <- NA
      source
      # nn_source <- source[nn_graph, ]
      # nn_source$group <- rep(seq_len(nrow(nn_graph)), 2)
      # nn_exclude <- source[-nn_index, ]
      # nn_exclude$group <- NA
      # rbind(nn_exclude, nn_source)
    })
    
    vegawidget::vw_shiny_set_data("chart", "projection", current_projection())
    
    output$chart <- vegawidget::renderVegawidget(spec)
  }
  
  ui <- ui_tour(history)
  shiny::shinyApp(ui, server)
}


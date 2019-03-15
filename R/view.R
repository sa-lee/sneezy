spec_tour <- function(data, half_range, dim_chart) {
  domain <- c(-half_range, half_range)
  
  tour_layer <- list(
    `$schema` = vegawidget::vega_schema("vega_lite"),
    width = dim_chart,
    height = dim_chart,
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
    
  vegawidget::as_vegaspec(tour_layer)
    
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

vw_tour <- function(data) {
  input <- setup_frames(data)
  spec_tour(input$data, input$half_range,  400)
}

# ui_tour <- function(history) {
#   d <- dim(history)
#   
#   ui_slider <-
#     shiny::sliderInput(
#       "slider",
#       label = "basis",
#       min = 1L,
#       max = d[3],
#       step = 1L,
#       value = 1L,
#       sep = "",
#       animate = shiny::animationOptions(interval = 500, loop = TRUE)
#     )
#   
#   shiny::fluidPage(
#     shiny::titlePanel("Tourring"),
#     shiny::sidebarLayout(
#       shiny::sidebarPanel(
#         ui_slider
#       ),
#       shiny::mainPanel(
#         vegawidget::vegawidgetOutput("chart")
#       )
#     )
#   )
# }

# shiny_tour <- function(data, tsne_coords, .subset) {
# 
#   server <- function(input, output) {
#     # reactives
#     
#     current_projection <- shiny::reactive({
#       source <- as.data.frame(tour_data %*% history[,,input$slider] / half_range)
#       source$group <- NA
#       source
#       # nn_source <- source[nn_graph, ]
#       # nn_source$group <- rep(seq_len(nrow(nn_graph)), 2)
#       # nn_exclude <- source[-nn_index, ]
#       # nn_exclude$group <- NA
#       # rbind(nn_exclude, nn_source)
#     })
#     
#     vegawidget::vw_shiny_set_data("chart", "projection", current_projection())
#     
#     output$chart <- vegawidget::renderVegawidget(spec)
#   }
#   
#   ui <- ui_tour(history)
#   shiny::shinyApp(ui, server)
# }


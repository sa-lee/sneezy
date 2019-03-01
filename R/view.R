spec_tour <- function(data, half_range, dim_chart) {
  domain <- c(-half_range, half_range)
  vegawidget::as_vegaspec(
    list(
      `$schema` = vegawidget::vega_schema("vega"),
      width = dim_chart,
      height = dim_chart,
      data = list(
        list(name = "projections", values = data),
        list(
          name = "thisKey",
          source =  "projections",
          transform = list(list(
            type = "filter",
            expr = "datum.key == currentKey"
          ))
        ),
        list(
          name= "prevKey",
          source = "projections",
          transform = list(list(
            type= "filter",
            expr = "datum.key == currentKey - stepKey"
          ))
        ),
        list(
          name= "nextKey",
          source= "projections",
          transform = list(list(
            type= "filter",
            expr= "datum.key == currentKey + stepKey"
          ))
        ),
        list(
          name = "points",
          source = "projections",
          transform = list( list(
            type= "aggregate",
            groupby= list("index")
          ))
        ),
        list(
          name= "interpolate",
          source= "points",
          transform= list(
            list(
              type= "lookup",
              from= "thisKey",
              key= "index",
              fields= list("index"),
              as= list("this"),
              default= NULL
            ),
            list(
              type= "lookup",
              from= "prevKey",
              key= "index",
              fields = list("index"),
              as= list("prev"),
              default=NULL
            ),
            list(
              type= "lookup",
              from= "nextKey",
              key= "index",
              fields= list("index"),
              as= list("next"),
              default= NULL
            ),
            list(
              type= "formula",
              as = "target_V2",
              expr = "interKey > currentKey ? datum.next.V2 : (datum.prev.V2||datum.this.V2)"
            ),
            list(
              type = "formula",
              as = "target_V1",
              expr =  "interKey > currentKey ? datum.next.V1 : (datum.prev.V1||datum.this.V1)"
            )
          )
        )
      ),
      signals = list(
        list(
          name = "minKey",
          value= 1
        ),
        list(
          name = "maxKey",
          value = 100
        ),
        list(
          name= "stepKey",
          value= 1
        ),
        list(
          name= "interKey",
          value= 1,
          on= list(list(
            events= "timer{1000}",
            update= "min(maxKey, currentKey + stepKey)"
          )
        )),
        list(
          name= "currentKey",
          value= 1,
          on= list(list(
            events = "timer{500}",
            update = "currentKey < maxKey ? currentKey + stepKey : minKey"
          ))
        )
      ),
      scales= list(
        list(
          name = "x",
          type= "linear",
          nice= TRUE,
          domain= domain,
          range= "width"
        ),
        list(
          name = "y",
          type= "linear",
          nice = TRUE,
          zero= FALSE,
          domain= domain,
          range = "height"
        )
      ),
      axes= list(
        list(
          title= "V1",
          orient= "bottom",
          scale= "x",
          grid= FALSE,
          tickCount = 5
        ),
        list(
          title= "V2",
          orient= "left",
          scale= "y",
          grid= FALSE,
          tickCount= 5
        )
      ),
      marks= list(
        list(
          name= "point",
          type= "symbol",
          from= list(
            data= "interpolate"
          ),
          encode= list(
            update= list(
              x= list(
                scale= "x",
                field= "target_V1"
              ),
              y= list(
                scale= "y",
                field= "target_V2"
              ),
              fillOpacity= 
                list(
                  value= 0.5
                ),
              fill= list(value= "black")
            )
          )
        )
      )
    )
  )
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


#'@export
setMethod("view_tour_xy",
          signature = "TourExperiment",
          function(.data, .on = NULL, .subset = NULL, clamp = FALSE, ...) {
            
            if (!requireNamespace("shiny", quietly = TRUE)) {
              stop("Please install shiny", call. = FALSE)
            }
            
            # -- data setup
            if (is.null(.on)) {
              # if .on is NULL we'll take the first basisSet
              .on <- basisSetNames(.data)[1]
            }
            projs <- basisSet(.data, .on)
            vals <- .retrieve_mat(.data, .on, .subset)
            
            if (is(vals, "LinearEmbeddingMatrix")) {
              vals <- sampleFactors(vals)
            }
            
            ui <- simple_ui()
            
            server <- tour_server(vals, projs)
            shiny::shinyApp(ui, server)
            
            
          })

simple_ui <- function() {
  shiny::fluidPage(
    shiny::actionButton("stream", "Turn stream on/off"),
    plotly::plotlyOutput("plot")
  )
}

setMethod("compute_half_range", 
          "ANY", 
          function(.data)   max(sqrt(rowSums(.data^2)))
)

setMethod("compute_half_range", 
          "LinearEmbeddingMatrix",
          function(.data) compute_half_range(sampleFactors(.data))
)

pl_axis <- function(half_range) {
  list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE,
    range = c(-half_range, half_range)
  )
}

tour_server <- function(vals, projs) {
  
  function(input, output, session) {
    
    # initial values
    half_range <- compute_half_range(vals)
    init <- vals %*% matrix(projs[,,1], nrow = ncol(vals), ncol = 2L)
    # initalise axis
    ax <- pl_axis(half_range)
    
    # initiate graph with initial values
    output$plot <- plotly::renderPlotly({
      plotly::toWebGL(
        plotly::layout(
          plotly::add_markers(
            plotly::plot_ly(x = init[,1], y = init[,2])
          ),
          xaxis = ax,
          yaxis = ax
        )
      )
    })
    
    # # reactiveValues() act very much like input values, but may be used to 
    # # maintain state (e.g., are we currently streaming?)
    # rv <- reactiveValues(
    #   stream = FALSE,
    #   yend = sum(yint), 
    #   n = length(yint)
    # )
    # 
    # # turn streaming on/off when the button is pressed
    # observeEvent(input$stream, {
    #   rv$stream <- if (rv$stream) FALSE else TRUE
    # })
    # 
    # observe({
    #   # if we're not streaming, don't do anything
    #   if (!rv$stream) return()
    #   
    #   # re-execute this code block to every 100 milliseconds
    #   invalidateLater(100, session)
    #   # changing a reactive value "invalidates" it, so isolate() is needed 
    #   # to avoid recursion
    #   isolate({
    #     rv$n <- rv$n + 1
    #     rv$yend <- rv$yend + sample(c(-1, 1), 1)
    #   })
    #   
    #   # add the new value to the plot
    #   plotlyProxy("plot", session) %>%
    #     plotlyProxyInvoke(
    #       "extendTraces", 
    #       list(
    #         y = list(list(rv$yend)), 
    #         x = list(list(rv$n))
    #       ), 
    #       list(0)
    #     )
    # })
    
  }
}




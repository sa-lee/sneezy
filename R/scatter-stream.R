#'@export
setMethod("view_tour_xy",
          signature = "TourExperiment",
          function(.data, .on = NULL, .subset = NULL, clamp = FALSE, ...) {
            
            if (!requireNamespace("shiny", quietly = TRUE)) {
              stop("Please install shiny", call. = FALSE)
            }
            
            # -- data setup, extracting the matrix
            vals <- .retrieve_mat(.data, .on, .subset)
            
            if (is(vals, "LinearEmbeddingMatrix")) {
              vals <- sampleFactors(vals)
            }
            
            # -- basis setup 
            if (is.null(.on)) {
              # if .on is NULL we'll take the first basisSet
              .on <- basisSetNames(.data)[1]
            }
            
            projs <- basisSet(.data, .on)
            
            # flatten into a list, this essentially so planned tour
            # works
            projs  <- Map(function(x) x[[1]], apply(projs, 3, list))
            
            angle <- 1/30
            
            # compute distance between realised bases
            dists <- vapply(seq.int(2, length(projs)), 
                            function(i) fproj_dist(projs[[i-1]], projs[[i]]),
                            numeric(1)
            )
            
            # steps for geodesic interpolation 
            steps <- sum(ceiling(dists/angle)) * 2
            
            # setup the callback
            plan <- tourr::new_tour(projs[[1]], tourr::planned_tour(projs))
            
            
            ui <- simple_ui()
            
            server <- tour_server(vals, plan, steps, angle)
            shiny::shinyApp(ui, server)
            
          })

simple_ui <- function() {
  shiny::fluidPage(
    shiny::actionButton("stream", "Turn stream on/off"),
    plotly::plotlyOutput("plot")
  )
}

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


tour_server <- function(vals, plan, start, steps, angle) {
  
  function(input, output, session) {
    
    # initial values
    half_range <- compute_half_range(vals)
    # initalise axis
    ax <- pl_axis(half_range)
    
    # graph shell
    output$plot <- plotly::renderPlotly({
      plotly::layout(
        plotly::plot_ly(type = "scattergl", mode = "markers"),
        xaxis = ax,
        yaxis = ax
      )
    })
    
    # # reactiveValues() act very much like input values, but may be used to 
    # # maintain state (e.g., are we currently streaming?)
    rv <- shiny::reactiveValues(
      stream = FALSE,
      init = start,
      n = 1,
      step =  plan(0)
    )
    # 
    # # turn streaming on/off when the button is pressed
    shiny::observeEvent(input$stream, {
      rv$stream <- if (rv$stream) FALSE else TRUE
    })

    shiny::observe({
      # if we're not streaming, don't do anything
      if (!rv$stream) return()
      if (rv$step$step == -1) return()
      
      # re-execute this code block to according to frame rate
      frame_rate <- 30
      
      shiny::invalidateLater(1000/frame_rate, session)
      # changing a reactive value "invalidates" it, so isolate() is needed
      # to avoid recursion
      shiny::isolate({
        print(rv$step$step)
        rv$init <- vals %*% rv$step$proj
        rv$n <- rv$n + 1
        rv$step <- plan(1/30)
      })

      # add the new value to the plot
      plotly::plotlyProxyInvoke(
        plotly::plotlyProxy("plot", session),
        "restyle",
        list(
          y = list(rv$init[,2]),
          x = list(rv$init[,1])
          # type = "scattergl",
          # mode = "markers"
        )
      )
      
      # # delete the previous trace
      # plotly::plotlyProxyInvoke(
      #   plotly::plotlyProxy("plot", session),
      #   "deleteTraces",
      #   0)
    })
    
  }
}





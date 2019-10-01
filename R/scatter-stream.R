#'@export
setMethod("view_tour_xy",
          signature = "TourExperiment",
          function(.data, .on = NULL, .subset = NULL, .color = NULL, clamp = FALSE, aps = 1, fps = 30,  ...) {
            
            .check_shiny()
            
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
            
            angle <- aps/fps
            
            # compute distance between realised bases
            dists <- vapply(seq.int(2, length(projs)), 
                            function(i) fproj_dist(projs[[i-1]], projs[[i]]),
                            numeric(1)
            )
            
            # steps for geodesic interpolation 
            steps <- sum(ceiling(dists/angle)) * 2
            
            # setup the callback
            plan <- tourr::new_tour(projs[[1]], tourr::planned_tour(projs))
            
            # initial projection
            start <- vals %*% projs[[1]]
            
            ui <- simple_ui()
            
            server <- tour_server(vals,  plan, .color, start, steps, angle, fps)
            shiny::shinyApp(ui, server)
            
          })

simple_ui <- function() {
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::actionButton("stream", "Turn stream on/off")
    ),
    shiny::fluidRow(
      shiny::column(8, plotly::plotlyOutput("plot")),
      shiny::column(2, plotly::plotlyOutput("axes"))
    )
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


tour_server <- function(vals, plan, .color, start, steps, angle, fps) {
  
  function(input, output, session) {
    
    # initial values
    half_range <- compute_half_range(vals)
    # initalise axis
    ax <- pl_axis(half_range)
    
    # axis shell
    output$axes <- plotly::renderPlotly(
      plotly::layout(
        plotly::plot_ly(),
        xaxis = ax,
        yaxis = ax
      )
    )
    
    # graph shell
    output$plot <- plotly::renderPlotly({
      plotly::layout(
        view_xy(start, .x = 1, .y = 2, .color = .color),
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
      frame_rate <- 1000/fps
      
      shiny::invalidateLater(frame_rate, session)
      # changing a reactive value "invalidates" it, so isolate() is needed
      # to avoid recursion
      shiny::isolate({
        rv$init <- vals %*% rv$step$proj
        rv$n <- rv$n + 1
        rv$step <- plan(angle)
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
        ),
        list(1)
      )
      
      # # delete the previous trace
      # plotly::plotlyProxyInvoke(
      #   plotly::plotlyProxy("plot", session),
      #   "deleteTraces",
      #   0)
    })
    
  }
}





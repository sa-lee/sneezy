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
            
            ui <- simple_ui()
            
            server <- tour_server(vals,  plan, .color, projs[[1]], steps, angle, fps)
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
  ax <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE
  )
  
  if (missing(half_range)) {
    return(ax)
  }
  
  c(ax, list(range = c(-half_range, half_range)))
}


init_axis_plot <- function(basis, cols) {
  nb <- nrow(basis) *2
  evens <- seq.int(2, nb*2, by = 2)
  x <- y <- rep(0, nb)
  x[evens] <- basis[,1]
  y[evens] <- basis[,2]
  p <- plotly::plot_ly(
    x = x, y = y, 
    type = "scatter", 
    mode = "lines", 
    color = I("black"), 
    showlegend = FALSE
    )
  p <-  plotly::add_annotations(p, 
                                x = basis[,1]*1.1,
                                y = basis[,2]*1.1,
                                text = cols,
                                xref = "x",
                                yref = "y",
                                showarrow = FALSE)
  ax <- pl_axis()
  p <- plotly::layout(p, xaxis = ax, yaxis = ax)
  plotly::config(p, displayModeBar = FALSE)
}

tour_server <- function(vals, plan, .color, start, steps, angle, fps) {
  
  function(input, output, session) {
    
    # initial values
    half_range <- compute_half_range(vals)
    # initalise axis
    ax <- pl_axis(half_range)
    
    init <- vals %*% start
    
    zeros <- rep(0, nrow(start))
    
    # axis shell
    output$axes <- plotly::renderPlotly(
      init_axis_plot(start, colnames(vals))
    )
    
    # graph shell
    output$plot <- plotly::renderPlotly({
      plotly::layout(
        view_xy(init, .x = 1, .y = 2, .color = .color),
        xaxis = ax,
        yaxis = ax
      )
    })
    
    # # reactiveValues() act very much like input values, but may be used to 
    # # maintain state (e.g., are we currently streaming?)
    rv <- shiny::reactiveValues(
      stream = FALSE,
      basis = start,
      proj = init,
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
        rv$basis <- rv$step$proj
        rv$proj <- vals %*% rv$step$proj
        rv$n <- rv$n + 1
        rv$step <- plan(angle)
      })

      # add the new value to the plot
      plotly::plotlyProxyInvoke(
        plotly::plotlyProxy("plot", session),
        "restyle",
        list(
          y = list(rv$proj[,2]),
          x = list(rv$proj[,1])
        ),
        list(1)
      )
      
      # restyle the axes
      output$axes <- plotly::renderPlotly(init_axis_plot(rv$basis, colnames(vals)))
    })
    
  }
}





#'@export
setMethod("view_tour_xy",
          signature = "TourExperiment",
          function(.data, .on = NULL, .subset = NULL, .color = NULL, clamp = TRUE, aps = 1, fps = 30,  ...) {
            
            .check_shiny()
            
            # -- data setup, extracting the matrix
            vals <- .retrieve_mat(.data, .on, .subset)
            
            if (is(vals, "LinearEmbeddingMatrix")) {
              vals <- SingleCellExperiment::sampleFactors(vals)
            }
            
            if (clamp) {
              vals <- .rescale(vals)
            }
            
            # -- basis setup 
            if (is.null(.on)) {
              # if .on is NULL we'll take the first basisSet
              .on <- basisSetNames(.data)[1]
            }
            
            projs <- basisSet(.data, .on)
            
            # flatten into a list, this essentially so planned tour
            # works
            projs  <- flatten_array(projs)
            
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
      shiny::column(4, 
                    shiny::actionButton("stream", icon = shiny::icon("play"), label = NULL),
                    shiny::actionButton("reset", icon = shiny::icon("undo"), label = NULL ),
                    plotly::plotlyOutput("axes")
      )
      ,
      shiny::column(8, plotly::plotlyOutput("plot"))
    ),
    shiny::fluidRow(
      shiny::column(12, plotly::plotlyOutput("path"))
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


init_axis_plot <- function(basis, cols, half_range) {
  x <- pad_zeros(basis[,1])
  y <- pad_zeros(basis[,2])
  p <- plotly::plot_ly(
    x = x, y = y, 
    type = "scatter", 
    mode = "lines", 
    color = I("black"), 
    showlegend = FALSE
    )
  p <-  plotly::add_text(p, 
                         x = basis[,1]*1.1,
                         y = basis[,2]*1.1,
                         text = cols, type = "text")
  ax <- pl_axis(half_range)
  p <- plotly::layout(p, xaxis = ax, yaxis = c(ax, list(scaleanchor = "x")))
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
      init_axis_plot(start, colnames(vals), half_range)
    )
    
    # tour shell
    output$plot <- plotly::renderPlotly({
      plotly::layout(
        view_xy(init, .x = 1, .y = 2, .color = .color),
        xaxis = ax,
        yaxis = c(ax, list(scaleanchor = "x")) # fixed aspect ratio
      )
    })
    
    # path shell
    output$path <- plotly::renderPlotly({
      plotly::layout(
        plotly::add_lines(
          plotly::plot_ly(type = "scatter", mode = "lines"),
          x = 1, 
          y = 1
        ),
        yaxis = pl_axis(),
        xaxis = list(
          title = "Step",
          showline = FALSE,
          zeroline = FALSE,
          showticklabels = TRUE,
          showgrid = FALSE
        )
      )
    })
    
    
    # # reactiveValues() act very much like input values, but may be used to 
    # # maintain state (e.g., are we currently streaming?)
    rv <- shiny::reactiveValues(
      stream = 0,
      basis = start,
      proj = init,
      n = 1,
      new_basis = TRUE,
      step =  plan(0)
    )
    
    # re-execute this code block to according to frame rate
    frame_rate <- 1000/fps
    # 
    # # turn streaming on/off when the button is pressed
    shiny::observeEvent(input$stream, {
      rv$stream <- 1 - rv$stream
      start_icon <- shiny::icon(c("play", "pause")[rv$stream + 1])
      shiny::updateActionButton(session, "stream", icon = start_icon)
    })

    shiny::observe({

      if (rv$stream == 0) return()
      
      # if (rv$step$step == -1) {
      #   shiny::isolate({
      #     rv$stream <- 1
      #   })
      #   return()
      # }
      shiny::invalidateLater(frame_rate, session)
      
      if (rv$stream == 1) {
        
        # changing a reactive value "invalidates" it, so isolate() is needed
        # to avoid recursion
        
        shiny::isolate({
          rv$basis <- rv$step$proj
          rv$proj <- vals %*% rv$step$proj
          rv$n <- rv$n + 1
          rv$new_basis <- rv$step$step == 0
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
        # add the new value to the plot
        plotly::plotlyProxyInvoke(
          plotly::plotlyProxy("axes", session),
          "restyle",
          list(
            y = list(pad_zeros(rv$basis[,2])),
            x = list(pad_zeros(rv$basis[,1]))
          ),
          list(0)
        )

        plotly::plotlyProxyInvoke(
          plotly::plotlyProxy("axes", session),
          "restyle",
          list(
            y = list(rv$basis[,2]*1.1),
            x = list(rv$basis[,1] * 1.1)
          ),
          list(1)
        )

        # add the new value to the plot
        plotly::plotlyProxyInvoke(
          plotly::plotlyProxy("path", session),
          "extendTraces",
          list(
            y = list(list(1)),
            x = list(list(rv$n))
          ),
          list(1)
        )
      }

      
    })
    
  }
}





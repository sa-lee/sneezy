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
                    plotly::plotlyOutput("axes")),
      shiny::column(8, plotly::plotlyOutput("plot"))
    ),
    shiny::fluidRow(
      shiny::column(12, plotly::plotlyOutput("path"))
    )
  )
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
    
    
    
    autoInvalidate <- shiny::reactiveTimer(1000/fps)
    
    rv <- shiny::reactiveValues(play = FALSE, step = plan(0))
    

    shiny::observeEvent(input$stream, {
      rv$play <- if (rv$play) FALSE else TRUE
    })
    
    
    # turn streaming on/off when the button is pressed
    shiny::observe({
      
      # reset if we have gotten to end of path
      if (rv$step$step == -1 || !rv$play) {
        return()
      }
      
      autoInvalidate()
      if (rv$play) {
        shiny::isolate({
          basis <- rv$step$proj
          proj <- vals %*% basis
          new_basis <- rv$step$step == 0
          rv$step <- plan(angle)
          # restyle traces
          restyle_points("plot", 1, session, proj)
          restyle_points("axes", 0, session, pad_zeros(basis))
          restyle_points("axes", 1, session, basis*1.1)
        })
        
      }
      
    })
    
  }
}





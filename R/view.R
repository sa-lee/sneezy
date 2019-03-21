spec_tour <- function(half_range) {
  domain <- c(-half_range, half_range)
  
  tour_layer <- list(
    # transform = list(
    #   list(filter =list(selection = "path"))
    # ),
    #selection = list(),
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
      color = list(
        condition = list(
          selection = "brush",
          value = "black"
        ),
        value = "gray")
    )
  )
  
  tour_layer
}


# Generates a vegaspec for a t-SNE/non-linear embedding layout
# - enable linked brushing between panels
# - toggle triangulation on/off
# - toggle centroids on/off
spec_projection <- function(coords) {
  half_range <- max(sqrt(rowSums(coords$Y^2)))
  domain <- c(-half_range, half_range)
  nl_layer <- list(
    title = paste("t-SNE with perplexity", coords$perplexity),
    mark = list(type = "circle", clip = TRUE),
    selection = list(brush = list(type = "interval")),
    encoding = list(
      
      x = list(field = "tsne_x", type = "quantitative", 
               scale = list(domain = domain),
               axis = list(title = NULL, 
                           grid = FALSE, 
                           ticks = FALSE,
                           labels = FALSE)
      ),
      y = list(field = "tsne_y", type = "quantitative",
               scale = list(domain = domain),
               axis = list(title = NULL, 
                           grid = FALSE, 
                           ticks = FALSE,
                           labels = FALSE)
      ),
      color = list(
        condition = list(
          selection = "brush",
          value = "black"
        ),
        value = "gray")
    )
  )
  nl_layer
}



spec_axes <- function(half_range) {
  domain <- c(-half_range, half_range)
  axes_layer <- list(
    `$schema` = vegawidget::vega_schema(),
    data = list(name = "rotations"),
    mark = list(type = "line", clip = TRUE),
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
      order = list(field = "group", type = "nominal"),
      color = list(value = "black")
    )
  )
  vegawidget::as_vegaspec(axes_layer)
}

spec_shep <- function(dist) {
  domain <- c(0, max(dist) + 0.1)
  shep_layer <- list(
    `$schema` = vegawidget::vega_schema(),
    data = list(name = "shep", values = dist),
    mark = list(type = "circle", clip = TRUE),
    encoding = list(
      x = list(field = "original", type = "quantitative",
               scale = list(domain = domain)),
      y = list(field = "embedding", type = "quantitative",
               scale = list(domain = domain)),
      color = list(value = "black"),
      opacity = list(value = 1 / 10)
    )
  )
  vegawidget::as_vegaspec(shep_layer)
}

sneezy <- function(data, max_bases, coords) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Please install shiny", call. = FALSE)
  }
  
  ui <- sneezy_ui(max_bases)
  server <- sneezy_server(data, max_bases, coords)
  
  shiny::shinyApp(ui, server)
  
}

sneezy_ui <- function(max_bases) {
  
  tour_slider <- shiny::sliderInput(
    "n_bases",
    label = "Number of bases",
    min = 1,
    value = 1,
    max = max_bases,
    step = 1,
    sep = "",
    animate = shiny::animationOptions(interval = 200, loop = TRUE)
  )
  
  
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(width = 12,
                   vegawidget::vegawidgetOutput("chart"),
                   shiny::fluidRow(
                     shiny::column(width = 4, 
                                   vegawidget::vegawidgetOutput("axes")
                     ),
                     shiny::column(width = 8,
                                   vegawidget::vegawidgetOutput("dist")
                     )
                   )
      ),
      tour_slider
    )
  )
  
}

sneezy_server <- function(data, max_bases, coords) {
  history <- basic_tour_path(data, max_bases = max_bases)
  # readust data
  tour_data <- tourr::rescale(data)
  tour_data <- scale(tour_data, center = TRUE, scale = FALSE)
  half_range  <- max(sqrt(rowSums(tour_data^2)))

  # static tsne_projection
  tbl_tsne <- data.frame(tsne_x = coords$Y[,1], 
                         tsne_y = coords$Y[,2])
  
  tbl_tour <- as.data.frame(tour_data %*% history[,,1])
  # initalise tbl
  tbl_init <- cbind(tbl_tour, tbl_tsne)
  
  tbl_zeros <- matrix(0, nrow = nrow(history[,,1]), ncol = 3)
  tbl_zeros[,3] <- seq_len(nrow(history[,,1]))
  
  
  # set up panels
  panel_tour <- spec_tour(half_range)
  panel_tsne <- spec_projection(coords)
  
  
  # projections + tsne spec + pairwise distance plots
  spec <- list(
    `$schema` = vegawidget::vega_schema(),
    data = list(name = "projections", values = tbl_init),
    hconcat = list(panel_tour, panel_tsne)
  )
  spec <- vegawidget::as_vegaspec(spec)
  
  spec_rotations <- spec_axes(half_range)
  
  spec_dist <- spec_shep(compute_flat_dist(data, coords))
  
  opt <- vegawidget::vega_embed(actions = FALSE)
  
  function(input, output) {
    
    output$dist <- vegawidget::renderVegawidget(
      vegawidget::vegawidget(spec_dist, embed = opt)
    )
    
    cur_path <- shiny::reactive({
      history[,,input$n_bases]
    })
    
    rct_axes <- shiny::reactive({
      path <- cur_path()
      path <- rbind(
        cbind(path, seq_len(nrow(path))),
        tbl_zeros
      )
      colnames(path) <- c("x", "y", "group")
      as.data.frame(path)
    })
    
    vegawidget::vw_shiny_set_data("axes", "rotations", rct_axes())
    
    output$axes <- vegawidget::renderVegawidget(
      vegawidget::vegawidget(spec_rotations, embed = opt)
    )
    
    rct_data <- shiny::reactive({
      tbl_init[, c(1,2)] <-tour_data %*% cur_path()
      tbl_init
    })
    
    vegawidget::vw_shiny_set_data("chart", "projections", rct_data())
    output$chart <- vegawidget::renderVegawidget(spec)
  }
  
}
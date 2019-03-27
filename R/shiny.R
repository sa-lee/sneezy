#' Exploring t-SNE embeddings interactively
#' 
#' @param data a data.frame or matrix
#' @param embedding list output from `Rtsne::Rtsne()`
#' @param max_bases number of bases for grand tour (see `tourr::save_history()`)
#' @param ... other stuff I want to include. 
#' 
#' @details 
#' Produces a 2 by 2 panel layout, where the upper left quadrant contains
#' 2-d projections from a grand tour (1), the upper right quadrant contains
#' the 2-d t-SNE embedding (2), the lower left quadrant contains strip plots
#' of each rescaled variable in `data` (3), the lower right quadrant contains
#' a scatter plot of distance in embedding space against distance in the data 
#' space (4).  
#' 
#' UI elements and interactions
#' 
#' A range slider is bound to the side bar panel, with a play button which
#' will animate the projections in (1).
#' 
#' There are two brushing selections available in (2): a mouse drag
#' on (2) will highlight selected points on (1) and (2) and (3). This
#' is useful for both seeing how clusters that form in embedding space move
#' in high-dimensional space and what variables define the cluster from 
#' (3). Pressing shift with a mouse drag will also highlight selected points
#' but this time revealing the simplex constructed on the embedding space
#' and how this simplex appears in high-dimensional space. 
#' 
#' The scatter plot (4) of inter point distances, can be used for checking
#' spurious embeddings... 
#' 
#' The strip plots can be used for highlighting point ranges ...
#'           
#' 
#' @return a shiny app
sneezy <- function(data, embedding, max_bases) {
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
    animate = shiny::animationOptions(interval = 250, loop = TRUE)
  )
  
  
  shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        tour_slider,
        vegawidget::vegawidgetOutput("axes")
      ),
      shiny::mainPanel(
        shiny::column(width = 8, vegawidget::vegawidgetOutput("chart")),
        shiny::column(width = 8, vegawidget::vegawidgetOutput("dist"))
      )
    )
  )
  
  
}

sneezy_server <- function(data, max_bases, coords) {
  history <- basic_tour_path(data, 
                             max_bases = max_bases)
  #path <- tourr::interpolate(history)
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
      tbl_init[, c(1,2)] <- tour_data %*% cur_path()
      tbl_init
    })
    
    vegawidget::vw_shiny_set_data("chart", "projections", rct_data())
    spec <- vegawidget::vegawidget(spec, embed = opt)
    output$chart <- vegawidget::renderVegawidget(spec)
  }
  
}
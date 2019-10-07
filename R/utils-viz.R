# Internals for interactivity and viz
pad_zeros <- function(x) {
  n <- nrow(x) * 2
  ans <- matrix(0, nrow = n, ncol = ncol(x))
  inx <- seq.int(2, n, by = 2)
  ans[inx, ] <- x
  ans  
}





# remove all axis ticks and grids 
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
  pad_basis <- pad_zeros(basis)
  x <- pad_basis[,1]
  y <- pad_basis[,2]
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

# proxyInvoke can be used for restyling a trace
restyle_points <- function(plot_id, trace_id, session, coords) {
  # add the new value to the plot
  plotly::plotlyProxyInvoke(
    plotly::plotlyProxy(plot_id, session),
    "restyle",
    list(
      y = list(coords[,2]),
      x = list(coords[,1])
    ),
    list(trace_id)
  )
}
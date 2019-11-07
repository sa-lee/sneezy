#' Compose tours side by side
#' 
#' @export
link_left <- function(x, y) {
  stopifnot(is(y, "gg"))
  stopifnot(is(x, "gif_image"))
  gif <- magick::image_read(x)
  
  dimension <- magick::image_info(gif[1])[, c("width", "height")]

  
  png <- tempfile(fileext = ".png")
  png(
    filename = png,
    width = dimension[["width"]],
    height = dimension[["height"]],
    units = "px"
  )
  print(y)
  dev.off()
  
  png <- magick::image_read(png)
  
  nrow <- nrow(magick::image_info(gif))
  
  new_gif <- magick::image_append(c(gif[1], png[1]))
  for (i in seq.int(2, nrow)) {
    combined <- magick::image_append(c(gif[i], png[1]))
    new_gif <- c(new_gif, combined)
  }
  new_gif
  
}

#' @export
link_right <- function(x,y) {
  link_left(y,x)
}
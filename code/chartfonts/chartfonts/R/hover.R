#' @title Set a font for a mouse hover
#' @description
#' Set parameters for font that will be used in a chart after mouse hover, e.g. when using package plotly.
#' @author from-to: 2020-10-13-
#' @keywords font
#' @return font properties
#' @export hover
#' @examples
#' \dontrun{
#'   hoverlabel = list(font = chartfonts::hover(), align = "left")
#' }

hover <- function() {
  list(
    size = 18,
    family = "Goudy Old Style",
    color = "rgb(205, 205, 205)"
  )
}


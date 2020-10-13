#' @title Set a font for a chart title
#' @description
#' Set parameters for font that will be used in a chart title, e.g. when using package plotly.
#' @author from-to: 2020-10-13-
#' @keywords font
#' @return font properties
#' @export title
#' @examples
#' \dontrun{
#'   plotly::layout(title = "Title text", font = chartfonts::title(), <rest_of_parameters>)
#' }

title <- function() {
  list(
    size = 18,
    family = "Calibri",
    color = "black"
  )
}


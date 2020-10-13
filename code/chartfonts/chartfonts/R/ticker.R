#' @title Set a font for a chart ticker
#' @description
#' Set parameters for font that will be used in a chart ticker, e.g. when using package plotly.
#' @author from-to: 2020-10-13-
#' @keywords font
#' @return font properties
#' @export ticker
#' @examples
#' \dontrun{
#'   xaxisProperties <- list(
#'     title = "Date",  # axis title
#'     titlefont = fontsUDP::DisplayFonts("axis"),
#'     showticklabels = T,
#'     tickformat = "%d %b %y",
#'     tickangle = 45,
#'     tickfont = chartfonts::ticker(),
#'     <rest_of_parameters>
#'   )
#' }

ticker <- function() {
  list(
    size = 14,
    family = "Constantia",
    color = "black"        # equivalent of white is #FFFFFF
  )
}


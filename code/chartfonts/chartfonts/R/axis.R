#' @title Set a font for a chart axis
#' @description
#' Set parameters for font that will be used in a chart axis, e.g. when using package plotly.
#' @author from-to: 2020-10-13-
#' @keywords font
#' @return font properties
#' @export axis
#' @examples
#' \dontrun{
#'   xaxisProperties <- list(title = "Date", titlefont = chartfonts::axis()), <rest_of_parameters>)
#' }

axis <- function() {
  list(
    size = 18,
    family = "Calibri",    # other font examples: "Arial, sans-serif", "Constantia", "Old Standard TT, serif" ...; more fonts can be taken e.g. from MS Word
    color = "black"        # other color examples: "lightgrey", "blue", "orange"
  )
}


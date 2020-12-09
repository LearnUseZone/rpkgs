# ####
# title:   Interactive plot with plotly
# author:  learnusezone@gmail.com
# from-to: 2020-12-09-
# purpose: interactive scatter plot with qualitative colorscales
# note:    used literature: https://plotly.com/r/line-and-scatter/
# ####


## ---- plotly_Rmd_input
# qualitative color scales
plotly::plot_ly(
  data = iris,
  x = ~Sepal.Length,
  y = ~Petal.Length,
  mode = "markers",
  type = "scatter",
  color = ~Species,
  colors = "Set2"
)


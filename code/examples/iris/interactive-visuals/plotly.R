# ####
# title:   Interactive plot of iris species by lengths
# author:  learnusezone@gmail.com
# from-to: 2020-12-19-
# purpose: interactive scatter plot with filters
# note:    -
# ####


## ---- plotly_Rmd_input
# load packages
library(dplyr)

# shared data
iris_shared <- crosstalk::SharedData$new(iris)

# species by color
color_scatter <- iris_shared %>%
  plotly::plot_ly(
    x = ~Sepal.Length,
    y = ~Petal.Length,
    mode = "markers",
    type = "scatter",
    color = ~Species,
    colors = "Set2",

    # hover text
    hoverinfo="text",     # suppress default text in tooltip
    text = ~base::paste(  # "\n" can be a part of text, e.g. "\ndate"
      base::paste("\n", "Iris species:  ", base::paste("<b>", Species, "</b>")),
      base::paste("\n", "Sepal length:", base::paste("<b>", `Sepal.Length`, "cm", "</b>")),
      base::paste("\n", "Petal length: ", base::paste("<b>", `Petal.Length`, "cm", "</b>"))
    ),
    hoverlabel = list(  # set properties of hover label text
      font = list(
        size = 16,
        family = "Arial",
        color = "black"
      ),
      align = "left"
    )
  ) %>% plotly::layout(
    title = "Iris species by lengths",
    xaxis = list(title = "Sepal length (cm)"),
    yaxis = list(title = "Petal length (cm)"),
    legend = list(
      orientation = "v",
      x = 0.82, y = 0.08,
      bgcolor = "rgb(245, 245, 245)",
      title=list(text = "Iris species"))
  )

# add a filter to an interactive qualitative color scales scatter plot
crosstalk::bscols(
  widths = c(2, NA),  # maximum sum of widths (without warning) = 12
  list(
    crosstalk::filter_slider("Sepal.Length", "Sepal length (cm)", iris_shared, ~Sepal.Length),
    crosstalk::filter_slider("Petal.Length", "Petal length (cm)", iris_shared, ~Petal.Length)
    # crosstalk::filter_slider("Sepal.Width", "Sepal width (cm)", iris_shared, ~Sepal.Width),
    # crosstalk::filter_slider("Petal.Width", "Petal width (cm)", iris_shared, ~Petal.Width)
  ),
  color_scatter
)


# ####
# title:   Interactive plot of iris species by lengths
# author:  learnusezone@gmail.com
# purpose: interactive scatter plot with filters
# note:    -
# ####


## ---- plotly_Rmd_input
# load packages
library(dplyr)

# shared data
iris_shared <- crosstalk::SharedData$new(iris)

# color by species
color_scatter <- iris_shared %>%
  plotly::plot_ly(
    x = ~ Sepal.Length,
    y = ~ Petal.Length,
    mode = "markers",
    type = "scatter",
    color = ~ Species,
    colors = c("blue", "red2", "green3"),  # alternative: "Set2"

    # hover text
    hoverinfo = "text", # suppress default text in tooltip
    text = ~ base::paste(
      paste("\n", "Iris species:  ", paste("<b>", Species, "</b>")),
      paste("\n", "Sepal length:", paste("<b>", `Sepal.Length`, "cm", "</b>")),
      paste("\n", "Petal length: ", paste("<b>", `Petal.Length`, "cm", "</b>"))
    ),
    hoverlabel = list(  # properties of hover label text
      font = list(
        size = 16,
        family = "Arial",
        color = "rgb(245, 245, 245)"
      ),
      align = "left"
    )
  ) %>%
  plotly::layout(
    title = "Iris species by lengths",
    xaxis = list(title = "Sepal length (cm)"),
    yaxis = list(title = "Petal length (cm)"),
    legend = list(
      orientation = "v",
      x = 0.82, y = 0.08,
      bgcolor = "white",  # alternative: rgb(245, 245, 245)
      title = list(text = "Iris species")
    )
  )

# add a filter to an interactive qualitative color scales scatter plot
crosstalk::bscols(
  widths = c(2, NA),
  list(
    crosstalk::filter_slider("Sepal.Length", "Sepal length (cm)", iris_shared, ~ Sepal.Length),
    crosstalk::filter_slider("Petal.Length", "Petal length (cm)", iris_shared, ~ Petal.Length)
  ),
  color_scatter
)


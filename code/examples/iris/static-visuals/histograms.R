# ####
# title:   Histograms of iris dataset
# author:  learnusezone@gmail.com
# purpose: static histograms of iris dataset
# note:    -
# ####


# histograms with different way of defining colors
graphics::par(lwd = 3)  # line width


# hist 1
graphics::hist(
  iris$Petal.Length,
  col = grDevices::rgb(0.9, 0.6, 0.3, 0.6),
  main = "Histogram of petal length",
  xlim = c(
    base::floor(min(iris$Petal.Length)),
    base::ceiling(max(iris$Petal.Length))
  ),
  ylim = c(0, 40),
  xlab = "Petal length (cm)",
  ylab = "Frequency",
  lwd = 2  # line width (axes)
)


# hist 2
graphics::hist(
  iris$Petal.Width,
  col = colors()[4],
  main = "Histogram of petal width",
  xlab = "Petal width (cm)",
  ylab = "Frequency",
)


# hist 3
graphics::hist(
  iris$Sepal.Length,
  col = 4,
  main = "Histogram of sepal length",
  xlab = "Sepal length (cm)",
  ylab = "Frequency",
)


# hist 4
graphics::hist(
  iris$Sepal.Width,
  col = "#ADD8E6",
  main = "Histogram of sepal width",
  xlab = "Sepal width (cm)",
  ylab = "Frequency",
)


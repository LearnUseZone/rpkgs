# ####
# title:   Histograms of iris dataset
# author:  learnusezone@gmail.com
# from-to: 2020-12-09-
# purpose: static histograms of iris dataset
# note:    -
# ####


# histograms with different way of defining colors
graphics::par(lwd = 3)  # line width (histogram border)


# hist 1
graphics::hist(
  iris$Petal.Length,
  col = grDevices::rgb(0.9, 0.6, 0.3, 0.6),
  main = "Histogram of Petal.Length",
  xlim = c(
    base::floor(base::min(iris$Petal.Length)),
    base::ceiling(base::max(iris$Petal.Length))
  ),
  ylim = c(0, 40),
  xlab = "Petal.Length (cm)",
  ylab = "Frequency of Petal.Length",
  lwd = 2  # line width (axes)
)


# hist 2
graphics::hist(iris$Petal.Width, col = colors()[4])


# hist 3
graphics::hist(iris$Sepal.Length, col = 4)


# hist 4
graphics::hist(iris$Sepal.Width, col = "#ADD8E6")


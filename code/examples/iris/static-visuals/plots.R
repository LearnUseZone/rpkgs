# ####
# title:   Plots of iris dataset
# author:  learnusezone@gmail.com
# from-to: 2020-12-09-
# purpose: static scatter plots of iris dataset
# note:    -
# ####


# plots
graphics::par(lwd = 1)  # line width (pairs, plot and legend; not axes)


# scatterplot matrix (plot, pairs)
graphics::pairs(iris[, -5], col = iris$Species,
                main = "Scatterplot matrix", oma = c(3, 3, 6, 12))
graphics::legend(0.82, 0.7,
                 legend = base::as.vector(base::unique(iris$Species)),
                 col = 1:3, pch = 19, bty = "n")


# scatter plot
graphics::plot(iris$Sepal.Length, iris$Petal.Length,
               col = iris$Species, main = "Sepal and Petal Length")
graphics::legend("topleft",
                 legend = base::as.vector(base::unique(iris$Species)),
                 col = 1:3, pch = 19, bty = "n")


# ####
# title:   Scatter plot matrix of iris dataset
# author:  learnusezone@gmail.com
# purpose: static scatter plot matrix of iris dataset
# note:    -
# ####


# plots
graphics::par(lwd = 1)  # chart lines widths


# scatter plot matrix
graphics::pairs(iris[, -5], col = c("blue", "red2", "green3")[iris$Species],
                main = "Scatter plot matrix",
                oma = c(3, 3, 5.5, 12.5))
graphics::legend(0.82, 0.7,      # "bottomright", ...
                 legend = base::as.vector(base::unique(iris$Species)),
                 col = c("blue", "red2", "green3"),
                 pch = 19,
                 bty = "n")


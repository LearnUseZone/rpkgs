# ####
# title:   Boxplots of iris dataset
# author:  learnusezone@gmail.com
# from-to: 2020-12-19-
# purpose: static boxplots of iris dataset
# note:    -
# ####


# load packages
library(ggplot2)

# boxplots
box_sl <- ggplot2::ggplot(iris) +
  geom_boxplot(aes(x = Species, y = Sepal.Length, color = Species),
               # outlier.colour = "brown",
               outlier.size = 3,
               # fill = "lightyellow",
               # alpha = 0.75,
               lwd = 1) +
  scale_color_manual(values = c("blue", "red2", "green3")) +
  geom_jitter(aes(x = Species, y = Sepal.Length),
              alpha = 0.6,
              col = "gray",
              shape = 1) +
  ggtitle("Sepal lengths \nof 3 Iris Species") +
  labs(y = "Sepal length (cm)") +
  theme_minimal() +
  theme(legend.position="none")

box_sw <- ggplot2::ggplot(iris) +
  geom_boxplot(aes(x = Species, y = Sepal.Width, color = Species),
               # outlier.colour = "brown",
               outlier.size = 3,
               # fill = "lightyellow",
               # alpha = 0.75,
               lwd = 1) +
  scale_color_manual(values = c("blue", "red2", "green3")) +
  geom_jitter(aes(x = Species, y = Sepal.Width),
              alpha = 0.6,
              col = "gray",
              shape = 1) +
  ggtitle("Sepal widths \nof 3 iris species") +
  labs(y = "Sepal width (cm)") +
  theme_minimal() +
  theme(legend.position="none")

gridExtra::grid.arrange(box_sl, box_sw, ncol = 2)


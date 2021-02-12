# ####
# title:   Interactive plot of iris species by lengths
# author:  learnusezone@gmail.com
# purpose: interactive scatter plot with filters
# note:    -
# ####


## ---- trelliscopejs_Rmd_input
# load packages
library(dplyr)
library(ggplot2)

# data manipulation for scatter plots using trelliscopejs
#   `x ` => independent x axis; `y ` => dependent y axis
#   combination 1
iris_SepalLength_SepalWidth <- iris %>%
  select(Sepal.Length, Sepal.Width, Species) %>%
  mutate(`x ` = "Sepal length (cm)", `y ` = "Sepal width (cm)") %>%
  rename(x = Sepal.Length,  y = Sepal.Width)

#   combination 2
iris_SepalLength_PetalLength <- iris %>%
  select(Sepal.Length, Petal.Length, Species) %>%
  mutate(`x ` = "Sepal length (cm)", `y ` = "Petal length (cm)") %>%
  rename(x = Sepal.Length, y = Petal.Length)

#   combination 3
iris_SepalLength_PetalWidth <- iris %>%
  select(Sepal.Length, Petal.Width, Species) %>%
  mutate(`x ` = "Sepal length (cm)", `y ` = "Petal width (cm)") %>%
  rename(x = Sepal.Length, y = Petal.Width)

#   combination 4
iris_SepalWidth_SepalLength <- iris %>%
  select(Sepal.Width, Sepal.Length, Species) %>%
  mutate(`x ` = "Sepal width (cm)", `y ` = "Sepal length (cm)") %>%
  rename(x = Sepal.Width, y = Sepal.Length)

#   combination 5
iris_SepalWidth_PetalLength <- iris %>%
  select(Sepal.Width, Petal.Length, Species) %>%
  mutate(`x ` = "Sepal width (cm)", `y ` = "Petal length (cm)") %>%
  rename(x = Sepal.Width, y = Petal.Length)

#   combination 6
iris_SepalWidth_PetalWidth <- iris %>%
  select(Sepal.Width, Petal.Width, Species) %>%
  mutate(`x ` = "Sepal width (cm)", `y ` = "Petal width (cm)") %>%
  rename(x = Sepal.Width, y = Petal.Width)

#   combination 7
iris_PetalLength_SepalLength <- iris %>%
  select(Petal.Length, Sepal.Length, Species) %>%
  mutate(`x ` = "Petal length (cm)", `y ` = "Sepal length (cm)") %>%
  rename(x = Petal.Length, y = Sepal.Length)

#   combination 8
iris_PetalLength_SepalWidth <- iris %>%
  select(Petal.Length, Sepal.Width, Species) %>%
  mutate(`x ` = "Petal length (cm)", `y ` = "Sepal width (cm)") %>%
  rename(x = Petal.Length, y = Sepal.Width)

#   combination 9
iris_PetalLength_PetalWidth <- iris %>%
  select(Petal.Length, Petal.Width, Species) %>%
  mutate(`x ` = "Petal length (cm)", `y ` = "Petal width (cm)") %>%
  rename(x = Petal.Length, y = Petal.Width)

#   combination 10
iris_PetalWidth_SepalLength <- iris %>%
  select(Petal.Width, Sepal.Length, Species) %>%
  mutate(`x ` = "Petal width (cm)", `y ` = "Sepal length (cm)") %>%
  rename(x = Petal.Width, y = Sepal.Length)

#   combination 11
iris_PetalWidth_SepalWidth <- iris %>%
  select(Petal.Width, Sepal.Width, Species) %>%
  mutate(`x ` = "Petal width (cm)", `y ` = "Sepal width (cm)") %>%
  rename(x = Petal.Width, y = Sepal.Width)

#   combination 12
iris_PetalWidth_PetalLength <- iris %>%
  select(Petal.Width, Petal.Length, Species) %>%
  mutate(`x ` = "Petal width (cm)", `y ` = "Petal length (cm)") %>%
  rename(x = Petal.Width, y = Petal.Length)

iris_long <- rbind(
  iris_SepalLength_SepalWidth, iris_SepalLength_PetalLength, iris_SepalLength_PetalWidth,
  iris_SepalWidth_SepalLength, iris_SepalWidth_PetalLength, iris_SepalWidth_PetalWidth,
  iris_PetalLength_SepalLength, iris_PetalLength_SepalWidth, iris_PetalLength_PetalWidth,
  iris_PetalWidth_SepalLength, iris_PetalWidth_SepalWidth, iris_PetalWidth_PetalLength)


# scatter plots with regression lines using trelliscopejs
iris_long %>%
  ggplot(aes(x = `x`, y = `y`)) +
  geom_point(color = "black", aes(shape = factor(1))) +
  scale_shape_manual(values = 1) +
  geom_vline(xintercept = 0:8, color = "#EEEEEE", size = 0.25) +
  geom_hline(yintercept = 0:8, color = "#EEEEEE", size = 0.25) +
  geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "orange") +
  geom_ribbon(method = "lm", formula = "y ~ x", stat = "smooth", fill = "orange", alpha = .4) +
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white")
  ) +
  trelliscopejs::facet_trelliscope(
    . ~ Species + `x ` + `y `,
    nrow = 1,
    ncol = 3,
    scales = c("same", "same"),
    name = "Regressions",
    path = "trelliscopejs/regressions",
    as_plotly = T,  # cause warning: https://github.com/hafen/trelliscopejs/issues/61
    data = iris_long
  )


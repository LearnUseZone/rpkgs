# ####
# title:   Data overview of iris dataset
# author:  learnusezone@gmail.com
# purpose: basic data overview of iris dataset
# note:    -
# ####


# variable names
var_names <- base::names(iris)

# class
class_name <- base::class(iris)

# dimensions (number of rows and columns)
dimensions <- base::dim(iris)

# head and tail
iris_head <- utils::head(iris, 4L)
iris_tail <- utils::tail(iris, 4L)

# summary
iris_summary <- base::summary(iris)


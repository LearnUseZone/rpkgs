# ####
# title:   Data overview of iris dataset
# author:  learnusezone@gmail.com
# from-to: 2020-12-09-
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
head_iris <- utils::head(iris, 4L)
tail_iris <- utils::tail(iris, 4L)

# summary
summary_iris <- base::summary(iris)


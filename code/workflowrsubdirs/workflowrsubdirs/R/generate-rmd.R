#' @title
#' Generate a temporary .Rmd file
#' @description
#' Generate a temporary (helping) .Rmd file, from its original .Rmd file
#' specified using input parameters "dir_path", "subdirs" and "patterns" of \code{\link{render_html}},
#' and temporarily save it into directory "analysis".
#' This temporarily saved .Rmd file will be used to generate final .html file and
#' will be deleted at the end of function \code{\link{render_html}} after final .html file is prepared.
#' This function requires that both input parameters are of length = 1 but no checks for that
#' is made directly inside this function because this is ensured using mapply() in \code{\link{render_html}}.
#' This function is called only from function \code{\link{render_html}} therefore
#' it's not exported and its input variables have no default values.
#' @param orig_rmd_path
#' character of length = 1
#' A path to original .Rmd files.
#' @param temp_rmd_path
#' character of length = 1
#' Names ("--" is a part of those names) of temporary .Rmd files.
#' @keywords workflowr, subdirectory
#' @return Temporarily saved .Rmd files.
#' @examples
#' \dontrun{
#'   generate_rmd("code-rmd/subdir/testfile.Rmd", "analysis/subdir--testfile.Rmd")
#' }

generate_rmd <- function(orig_rmd_path, temp_rmd_path) {
  base::cat(
    "---\n",
    # YAML header copied (except comments) from an original .Rmd file
    yaml::as.yaml(rmarkdown::yaml_front_matter(orig_rmd_path)),
    "---\n\n",
    "**Source file:** ", orig_rmd_path,
    "\n\n",

    # r chunk code (not YAML header)
    #   "output.dir" returns directory "analysis" in this case (also tested) and "../" goes one directory up
    "```{r child = base::file.path(knitr::opts_knit$get(\"output.dir\"), \"../", orig_rmd_path, "\")}\n```",

    file = temp_rmd_path,  # file = a name of file that will be created
    sep = "",
    append = F
  )
}

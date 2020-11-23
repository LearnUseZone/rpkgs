#' @title
#' Generate temporary .Rmd files
#' @description
#' Generate temporary .Rmd files from their original .Rmd files,
#' specified using input parameters "dir_path", "subdirs" and "patterns" of \code{\link{render_html}},
#' and (temporarily) save them into directory "analysis".
#' These temporarily saved .Rmd files will be used to generate final .html file.
#' These generated .Rmd files will be deleted at the end of function \code{\link{render_html}}
#' after final .html files are rendered.
#' This function is called only from function \code{\link{render_html}} therefore
#' it's not exported and its input variables have no default values.
#' @param orig_rmd_path
#' character
#' Paths to original .Rmd files.
#' @param temp_rmd_path
#' character
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

#' @title
#' Generate temporary .Rmd files
#' @description
#' Generate temporary .Rmd files from their original .Rmd files,
#' usually saved in directory "code" and its subdirectories,
#' and save them into directory "analysis".
#' These generated .Rmd files will be deleted at the end of function \code{\link{render_html}}
#' after final .html files are rendered.
#' This function is meant to be called only from function \code{\link{render_html}} so
#' it will be not exported therefore input variables have no default values.
#' @param orig_rmd_path
#' character
#' Path to an original .Rmd file saved in workflowr subdirectory.
#' @param temp_rmd_path
#' character
#' Name ("--" is usually part of it's name) of a temporary .Rmd file that will be temporarily saved into directory "analysis".
#' This temporary file is generated from its original .Rmd file specified in path_Rmd, then it will be deleted within \code{\link{render_html}}.
#' @keywords workflowr, subdirectory
#' @return Temporary .Rmd file, from its original .Rmd file saved in a subdirectory, used to generate final .html file.
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

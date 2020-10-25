#' @title Generate temporary .Rmd files
#' @description
#' It generates .Rmd files from their original .Rmd files that are saved in subdirectories and saves them into directory "analysis".
#' These generated .Rmd files are meant to be temporary and they will be deleted at the end of function \code{\link{generate_html}} after final .html files are rendered.
#' @param dir
#' character (default: "code-Rmd").
#' Path to a directory, under a main workflowr subdirectory, where original Rmd files are saved.
#' @param path_Rmd
#' character (default: NULL).
#' Path to an original .Rmd file. Path ignores dir (see the 1st parameter).
#' @param temp_name_Rmd
#' character (default: NULL).
#' Name ("--" is usually part of it's name) of a temporary .Rmd file that will be temporarily saved into directory "analysis".
#' This temporary file is generated from its original .Rmd file specified in path_Rmd, then it will be deleted within \code{\link{generate_html}}.
#' @keywords workflowr, subdirectory
#' @return Temporary .Rmd file, from its original .Rmd file saved in a subdirectory, used to generate final .html file.
#' @examples
#' \dontrun{
#'   generate_rmd("code-Rmd", "subPages1/testPrint1.Rmd", "subPages1--testPrint1.Rmd")
#' }

generate_rmd <- function(dir = "code-Rmd", path_Rmd = NULL, temp_name_Rmd = NULL) {
  rel_path <- base::file.path(".", path_Rmd)  # relative path to an original .Rmd file that will be rendered to .html file inside function wflow_build_dir(), "." is used for setting a correct path in parameter "child" of "r chunk" below
  base::cat(
    "---\n",
    yaml::as.yaml(rmarkdown::yaml_front_matter(rel_path)),  # YAML header from an original .Rmd file
    "---\n\n",
    "**Source file:** ", path_Rmd,            # link to original .Rmd file from workflowr subdirectory
    "\n\n",

    # r chunk code (not YAML header); [lit 4]
    "```{r child = base::file.path(knitr::opts_knit$get(\"output.dir\"), \".", rel_path, "\")}\n```",

    file = base::file.path("analysis", temp_name_Rmd),  # file = a name of file that will be created
    sep = "",
    append = F  # overwrite a content of a file
  )
  # Notes #
  # Explanation of usage dot defined by \"." in r child in r chunk code
  #   - A dot defined by \"." is required because knitr::opts_knit$get(\"output.dir\") returns "analysis"
  #     as output directory in this case so "child" parameter of "r chunk"
  #     has to 1stly go one directory up (rel_path starts with "./" + following \"." => ../)
}

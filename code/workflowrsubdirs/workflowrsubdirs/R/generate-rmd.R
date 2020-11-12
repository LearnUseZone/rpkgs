#' @title
#' Generate temporary .Rmd files
#' @description
#' Generate temporary .Rmd files from their original .Rmd files,
#' usually saved in directory "code" and its subdirectories,
#' and save them into directory "analysis".
#' These generated .Rmd files will be deleted at the end of function \code{\link{generate_html}}
#' after final .html files are rendered.
#' @param orig_rmd_path
#' character
#' Path to an original .Rmd knitr file.
#' @param temp_rmd_path
#' character
#' Name ("--" is usually part of it's name) of a temporary .Rmd file that will be temporarily saved into directory "analysis".
#' This temporary file is generated from its original .Rmd file specified in path_Rmd, then it will be deleted within \code{\link{generate_html}}.
#' @keywords workflowr, subdirectory
#' @return Temporary .Rmd file, from its original .Rmd file saved in a subdirectory, used to generate final .html file.

generate_rmd <- function(orig_rmd_path, temp_rmd_path) {
  orig_rmd_rel_path <- base::file.path(".", orig_rmd_path)  # relative path to an original .Rmd file, "." is used for setting a correct path in parameter "child" of "r chunk" below
  base::cat(
    "---\n",
    yaml::as.yaml(rmarkdown::yaml_front_matter(orig_rmd_rel_path)),  # YAML header from an original .Rmd file
    "---\n\n",
    "**Source file:** ", orig_rmd_path,  # link to original .Rmd file from workflowr subdirectory
    "\n\n",

    # r chunk code (not YAML header); [lit 4]
    "```{r child = base::file.path(knitr::opts_knit$get(\"output.dir\"), \".", orig_rmd_rel_path, "\")}\n```",  # output.dir = directory "analysis" (in this case)
    file = temp_rmd_path,  # file = a name of file that will be created
    sep = "",
    append = F
  )
  # Notes
  #   Explanation of usage dot defined by \"." in r child in r chunk code
  #    - A dot defined by \"." is required because knitr::opts_knit$get(\"output.dir\") returns "analysis"
  #      as output directory in this case so "child" parameter of "r chunk"
  #      has to 1stly go one directory up (orig_rmd_rel_path starts with "./" + following \"." => ../)
}

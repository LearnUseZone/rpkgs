#' @title
#' Generate .html files from their associate .Rmd files
#' @description
#' Process only those .Rmd files (based on input parameters) that meet required criteria.
#' @param dir_path
#' character of length = 1 (default: "code-rmd").
#' Path to a subdirectory, under a main workflowr directory, where .Rmd files for rendering are saved.
#' A directory name can be first instead of using "./" etc. (e.g. "code-rmd" instead of "./code-rmd").
#' Examples:
#' dir_path = "code-rmd"
#' dir_path = c("code-rmd/subdir1\\subdir2")
#' @param subdirs
#' logical of length = 1 (default: TRUE)
#' If TRUE, file listing will recurse into directories in parameter dir.
#' If FALSE, file listing will be only directly from directories in parameter dir.
#' @param patterns
#' character of length > 0 or NULL (default: NULL)
#' A character vector of paths to .Rmd files for rendering.
#' If NULL, process all .Rmd files based values in parameters dir and subdirs.
#' If not NULL, process files matching a regular expression.
#' Examples:
#' patterns = "^.*page.*.\[  R , r \]md$")
#' patterns = c("page1.Rmd", ".*page2.*.Rmd")
#' @param commit
#' character (default: FALSE)
#' If TRUE, a separate commit of temporary .Rmd files (temporary saved in "analysis") is created.
#' It's suggested to use "commit = TRUE" only after original .Rmd files saved
#' in subdirectories are tested properly and so are completely ready,
#' otherwise there could be uselessly many commits.
#' @keywords workflowr, subdirectory
#' @return Final .html files from their original .Rmd files saved in subdirectories.
#' @export generate_htmls
#' @examples
#' \dontrun{
#'   generate_htmls()
#'   generate_htmls(dir_path = c("code-rmd\\subdir"), subdirs = F)
#'   generate_htmls("code-rmd/subdir", T, c("file1.Rmd", "-.*.[ R , r ]md"))
#' }

generate_htmls <- function(dir_path = "code-rmd", subdirs = T, patterns = NULL, commit = F) {
  # initial settings
  base::setwd(here::here())  # a project working directory could be changed after opening .Rproj
  initial_checks(dir_path, subdirs, patterns)
  dir_path <- base::gsub("\\\\", "/", dir_path)  # clearer to work (with one type of slash) with "/"

  orig_rmd_paths <- create_rmd_paths(dir_path, subdirs, patterns)
  render_to_htmls(orig_rmd_paths, commit)
}

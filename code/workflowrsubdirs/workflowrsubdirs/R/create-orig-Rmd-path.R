#' @title
#' Create paths to original .Rmd files for future rendering
#' @description
#' Create paths to original .Rmd files for future rendering into .html.
#' If no file for rendering is found, processing ends.
#' @param dirs
#' character (default: "code-rmd").
#' Paths to directories, under a main workflowr directory, where original .Rmd files are saved.
#' Examples:
#' dirs = "code-rmd"
#' dirs = c("code-rmd/subpage", "code-rmd/subpage1\\subpage2")
#' @param subdirs
#' logical (default: TRUE).
#' If TRUE, file listing will also recurse into directories in parameter dir.
#' If FALSE, file listing will be only directly from directories in parameter dir.
#' @param orig_rmd_pattern
#' character (default: NULL). Vector of paths to original .Rmd files.
#' If NULL, process all .Rmd files based values in parameters dir and subdirs.
#' If not NULL, process files matching written regular expression.
#' Examples:
#' orig_rmd_pattern = "^.*page.*.\[  R , r \]md$")
#' orig_rmd_pattern = c("page1.Rmd", ".*page2.*.Rmd")
#' @keywords workflowr, subdirectory
#' @return Character vector "orig_rmd_path" but stop processing if no file meets criteria.
#' @examples
#' \dontrun{
#'   create_orig_rmd_path(dirs = "code-rmd", subdirs = T, orig_rmd_pattern = ".*page.*.(R|r)md$")
#' }

create_orig_rmd_path <- function(dirs = "code-rmd", subdirs = T, orig_rmd_pattern = NULL) {
  # variable initialization
  orig_rmd_path <- c()  # return variable for created original .Rmd paths; initialization required because of append() below

  # initial settings based on orig_rmd_pattern for mapply() below
  ## solving: orig_rmd_pattern == NULL
  if (base::is.null(orig_rmd_pattern)) orig_rmd_pattern = "(?i)^.*\\.rmd$"  # if orig_rmd_pattern == NULL then search for all files in set directory in dir


  # generate a character vector of .Rmd files for further rendering
  ## create a character vector or a list of visible files #
  result_orig_rmd_pattern <- c()
  for (iterate_dirs in 1:base::length(dirs)) {
    result_orig_rmd_pattern <- try({  # result_of_try <- try (...) would be the same as result_orig_rmd_pattern in this case; it's better to have result_orig_rmd_pattern <- try, in case that something in mapply() fails
      base::mapply(
        base::list.files,    # if some file doesn't exist then list.files() produces list (instead of a character vector)
        path = dirs[iterate_dirs],  # path consisting of directories and subdirectories; lf = list_files
        full.names = T,      # example of a full name: code-rmd/subPages/test.Rmd
        recursive = subdirs,
        pattern = orig_rmd_pattern
        # Notes
        #   all.files = F    # only visible files are processed
        #   recursive = F    # listing will not recurse into subdirectories
        #   include.dirs = T # include a subdirectory that matches a regular expression in orig_rmd_pattern
      )
    })
    result_orig_rmd_pattern <- base::unname(base::unlist(result_orig_rmd_pattern))  # remove unwanted list elements if some of orig_rmd_pattern doesn't exist; also if result_orig_rmd_pattern is a matrix, size will be lowered
    orig_rmd_path <- base::append(orig_rmd_path, result_orig_rmd_pattern)           # append() - because each for () cycle may generate new result_orig_rmd_pattern
  } # for (iterate_dirs in 1:base::length(dir))

  if (length(orig_rmd_path) == 0) stop("No file meets criteria. Processing ends.")

  # return a vector of real paths of original rmd files under directory in dir
  return(orig_rmd_path)


  # Notes
  #   If there's more complex regular expression like the following one, use package "stringr" because it solves e.g. problems with escaping "]" that package function like "grepl()" has.
  #     if (!stringr::str_detect(orig_rmd_pattern[iteration_path_rmd], "(?i)^.*\\.[\\(, \\[]?\\s*r\\s*[\\,, \\|]?\\s*r?\\s*[\\), \\]]?md\\$$"))
}

#' @title
#' Create paths to original .Rmd files for future rendering
#' @description
#' Create paths to original .Rmd files for future rendering into .html.
#' If no file for rendering is found, processing ends.
#' This function is meant to be called only from function \code{\link{generate_html}} so
#' it will be not exported therefore input variables have no default values.
#' @param dirs
#' character
#' Paths to directories, under a main workflowr directory, where original .Rmd files are saved.
#' Examples:
#' dirs = "code-rmd"
#' dirs = c("code-rmd/subpage", "code-rmd/subpage1\\subpage2")
#' @param subdirs
#' logical
#' If TRUE, file listing will also recurse into directories in parameter dir.
#' If FALSE, file listing will be only directly from directories in parameter dir.
#' @param orig_rmd_patterns
#' character
#' Vector of paths to original .Rmd files.
#' If NULL, process all .Rmd files based values in parameters dir and subdirs.
#' If not NULL, process files matching written regular expression.
#' Examples:
#' orig_rmd_pattern = "^.*page.*.\[  R , r \]md$")
#' orig_rmd_pattern = c("page1.Rmd", ".*page2.*.Rmd")
#' @keywords workflowr, subdirectory
#' @return Character vector "orig_rmd_path" but stop processing if no file meets criteria.
#' @examples
#' \dontrun{
#'   create_orig_rmd_path(dirs = "code-rmd", subdirs = T, orig_rmd_patterns = ".*page.*.(R|r)md$")
#' }

create_orig_rmd_path <- function(dirs, subdirs, orig_rmd_patterns) {
  # variable initialization
  orig_rmd_path <- c()  # return variable for created original .Rmd paths; initialization required because of append() below

  # initial settings based on orig_rmd_patterns for mapply() below
  ## solving: orig_rmd_patterns == NULL
  if (base::is.null(orig_rmd_patterns)) orig_rmd_patterns = "(?i)^.*\\.rmd$"  # if orig_rmd_patterns == NULL then search for all files in set directory in dir


  # generate a character vector of .Rmd files for further rendering
  ## create a character vector or a list of visible files #
  result_orig_rmd_patterns <- c()
  for (iterate_dirs in 1:base::length(dirs)) {
    result_orig_rmd_patterns <- try({  # result_of_try <- try (...) would be the same as result_orig_rmd_patterns in this case; it's better to have result_orig_rmd_patterns <- try, in case that something in mapply() fails
      base::mapply(
        base::list.files,    # if some file doesn't exist then list.files() produces list (instead of a character vector)
        path = dirs[iterate_dirs],  # path consisting of directories and subdirectories; lf = list_files
        full.names = T,      # example of a full name: code-rmd/subPages/test.Rmd
        recursive = subdirs,
        pattern = orig_rmd_patterns
        # Notes
        #   all.files = F    # only visible files are processed
        #   recursive = F    # listing will not recurse into subdirectories
        #   include.dirs = T # include a subdirectory that matches a regular expression in orig_rmd_patterns
      )
    })
    result_orig_rmd_patterns <- base::unname(base::unlist(result_orig_rmd_patterns))  # remove unwanted list elements if some of orig_rmd_patterns doesn't exist; also if result_orig_rmd_patterns is a matrix, size will be lowered
    orig_rmd_path <- base::append(orig_rmd_path, result_orig_rmd_patterns)            # append() - because each for () cycle may generate new result_orig_rmd_patterns
  } # for (iterate_dirs in 1:base::length(dir))

  if (length(orig_rmd_path) == 0) stop("No file meets criteria. Processing ends.")
  orig_rmd_path <- base::unique(orig_rmd_path)  # if orig_rmd_patterns contains link to the same files then those files have to be processed only once, otherwise such files will be build (with wflow_build()) only once but following warning message pops-up: cannot remove file xxx, reason 'No such file or directory'

  # return a vector of real paths of original rmd files under directory in dir
  return(orig_rmd_path)
}

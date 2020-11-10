#' @title
#' Create paths to original .Rmd files for future rendering
#' @description
#' Create paths to original .Rmd files for future rendering otherwise stop processing.
#' @param dirs
#' character (default: "code-Rmd").
#' Path to a directory, under a main workflowr subdirectory, where original Rmd files are saved.
#' @param subdirs
#' character (default: NULL). It's case insensitive.
#' If only_subdirs == NULL then all subdirectories and files within directory in input parameter dir are processed, otherwise only files in subdirectories in this input parameter only_subdirs are processed.
#' If only_subdirs != NULL then it's a vector of subdirectories in directory specified in input parameter dir.
#' Examples: only_subdirs = NULL; only_subdirs == c("subdir1", "subdir.Rmd")
#' @param orig_rmd_pattern
#' character (default: NULL).
#' If orig_rmd_pattern == NULL then search for all files in directories set by dir or only_subdirs.
#' Vector of paths to original .Rmd files. These file paths start with a name of the 1st subdirectory of a directory specified in variable "dir".
#' Example when directories subPagesX are saved in directory dir = "code-Rmd":
#' file_path = c("subPages2/testPrint1.Rmd", "subPages3/testPrint2.Rmd")
#' file_path = c("subPages2\\testPrint1.Rmd", "subPages3\\testPrint2.Rmd")
#' @keywords workflowr, subdirectory
#' @return None, but stop processing if any of checks fails.
#' @examples
#' \dontrun{
#'   initial_checks(dir, path_orig_Rmd)
#' }

create_orig_rmd_path <- function(dirs = "code-Rmd", subdirs = T, orig_rmd_pattern = NULL) {
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
        full.names = T,      # example of a full name: code-Rmd/subPages/test.Rmd
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

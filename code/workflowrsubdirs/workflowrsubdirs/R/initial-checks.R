#' @title
#' Check rules for directories and subdirectories
#' @description
#' Check rules for directories and subdirectories to evaluate if rendering of .html files is possible.
#' This function is meant to be called only from function \code{\link{generate_html}} so
#' it will be not exported therefore input variables have no default values.
#' @param dirs
#' character
#' Path to a directory, under a main workflowr subdirectory, where original Rmd files are saved.
#' @param subdirs
#' character
#' Paths to directories, under a main workflowr directory, where original .Rmd files are saved.
#' Examples:
#' dirs = "code-rmd"
#' dirs = c("code-rmd/subpage", "code-rmd/subpage1\\subpage2")
#' @param orig_rmd_pattern
#' character
#' Vector of paths to original .Rmd files.
#' If NULL, process all .Rmd files based values in parameters dir and subdirs.
#' If not NULL, process files matching written regular expression.
#' Examples:
#' orig_rmd_pattern = "^.*page.*.\[  R , r \]md$")
#' orig_rmd_pattern = c("page1.Rmd", ".*page2.*.Rmd")
#' @keywords workflowr, subdirectory
#' @return None but stop processing if no file meets criteria.
#' @examples
#' \dontrun{
#'   initial_checks(dirs = "code-rmd", subdirs = T, orig_rmd_pattern = ".*page.*.(R|r)md$")
#' }

initial_checks <- function(dirs, subdirs, orig_rmd_pattern) {
  # check an existence of a user chosen directories
  if (is.null(dirs)) stop ("At least one directory is required.")  # solving: dirs != NULL

  # check if at least one directory in dirs exists and if non of default workflowr directories is chosen
  dirs_count <- 0  # number of (any) existing directories in "dirs"
  for (iterate_dirs in 1:base::length(dirs)) {
    if (base::file.exists(dirs[iterate_dirs])) dirs_count <- dirs_count + 1
    if (dirs[iterate_dirs] %in% c("analysis", "code", "data", "output", "public")) {
      stop(base::paste0("Choose other than default workflowr directory."))
    }
  }
  if (dirs_count == 0) stop("Non of specified directories exist in main workflowr directory.")


  # check input parameter subdirs
  if (!((subdirs == F || subdirs == T) && base::length(subdirs) == 1)) {
    stop("Input parameter subdirs can be only FALSE or TRUE. Processing ends.")
  }


  # check .Rmd files in directories analysis and input variable dir
  #   ensure that there are no temporary .Rmd files in directory "analysis" otherwise you may receive message like following one after trying to run function wflow_git_commit(...): Error: Commit failed because no files were added. Attempted to commit the following files: (list of file paths) Any untracked files must manually specified even if `all = TRUE`.
  if (base::length(
    double_hyphen_paths <- base::dir(
      path = "analysis",
      pattern = "(?i)^.*\\-\\-.*.rmd",
      full.names = T,
      recursive = T
    )) > 0) {  # if some "*--*.Rmd" file was found

    base::cat(
      "Following .Rmd files contain \"--\" which is not allowed in directory \"analysis\":\n",
      double_hyphen_paths,
      "\nPlease choose one of the following options:",
      "1 - Files will be deleted automatically and rendering of .Rmd files will continue.",
      "2 - Rendering of .Rmd files will stop and I will take care of relevant .Rmd files manually.",
      sep = "\n"
    )
    option <- base::readline(prompt = "Choose 1 or 2: ")
    if (option == 1) {
      base::file.remove(double_hyphen_paths)
    } else if (option == 2) {
      base::stop("Processing ends based on your chosen option.")
    } else (
      base::stop("Processing ends because you chose not available option.")
    )
  }
}

#' @title
#' Check rules for directories and subdirectories
#' @description
#' Check rules for directories and subdirectories to evaluate if rendering of .html files is possible.
#' @param dirs
#' character (default: "code-rmd").
#' Path to a directory, under a main workflowr subdirectory, where original Rmd files are saved.
#' @param subdirs
#' character (default: "code-rmd").
#' Paths to directories, under a main workflowr directory, where original .Rmd files are saved.
#' Examples:
#' dirs = "code-rmd"
#' dirs = c("code-rmd/subpage", "code-rmd/subpage1\\subpage2")
#' @param orig_rmd_pattern
#' character (default: NULL). Vector of paths to original .Rmd files.
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

initial_checks <- function(dirs = "code-rmd", subdirs = T, orig_rmd_pattern = NULL) {
  # check an existence of a user chosen directories
  if (is.null(dirs)) stop ("At least one directory is required.")  # solving: dirs != NULL

  # check if non of default workflowr directories is chosen
  dirs_count <- 0  # how many directories in parameter "dir" exists
  for (iterate_dirs in 1:base::length(dirs)) {
    if (base::file.exists(dirs[iterate_dirs])) dirs_count <- dirs_count + 1
    if (dirs[iterate_dirs] %in% c("analysis", "code", "data", "output", "public")) {
      stop(base::paste0("Choose other than default workflowr directory."))
    }
  }
  if (dirs_count == 0) stop("Non of specified directories exist in main workflowr directory.")

  # check input parameter subdirs
  if (base::is.null(subdirs)) stop("Input parameter subdirs cannot be NULL. Processing ends.")
  # add check if subdirs is only one logical (vector of length 1)???


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

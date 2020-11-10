#' @title
#' Create paths to original .Rmd files for future rendering
#' @description
#' Check if rendering .html files from .Rmd files from subdirectories is possible.
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

initial_checks <- function(dirs = "code-Rmd", subdirs = T, orig_rmd_pattern = NULL) {
  # check an existence of a user chosen directories
  if (is.null(dirs)) stop ("At least one directory is required.")  # solving: dirs != NULL

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

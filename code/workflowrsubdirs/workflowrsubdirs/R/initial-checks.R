#' @title
#' Check rules for directories and files
#' @description
#' Check rules for directories and .Rmd files to evaluate if rendering of .html files is possible.
#' This function is meant to be called only from function \code{\link{generate_html}} so
#' it will be not exported therefore input variables have no default values.
#' @param dirs
#' character
#' Path to a directory, under a main workflowr subdirectory, where original Rmd files are saved.
#' It can be only of length = 1.
#' Examples:
#' dirs = "code-rmd"
#' dirs = c("code-rmd/subdir1\\subdir2")
#' @param subdirs
#' logical
#' It can be only of length = 1.
#' If TRUE, file listing will also recurse into directories in parameter dir.
#' If FALSE, file listing will be only directly from directories in parameter dir.
#' @param orig_rmd_patterns
#' character
#' Vector of paths to original .Rmd files.
#' If NULL, process all .Rmd files based values in parameters dir and subdirs.
#' If not NULL, process files matching written regular expression.
#' Examples:
#' orig_rmd_patterns = "^.*page.*.\[  R , r \]md$")
#' orig_rmd_patterns = c("page1.Rmd", ".*page2.*.Rmd")
#' @keywords workflowr, subdirectory
#' @return None but stop processing if no file meets criteria.
#' @examples
#' \dontrun{
#'   initial_checks(dirs = "code-rmd", subdirs = T, orig_rmd_pattern = ".*page.*.(R|r)md$")
#' }

initial_checks <- function(dirs, subdirs, orig_rmd_patterns) {
  # check an existence of a user chosen directory
  if (base::is.null(dirs)) stop ("Parameter dirs cannot be NULL. Processing ends.", call. = F)  # solving: dirs != NULL
  if (dirs == "") stop("Parameter dirs cannot be empty string. Processing ends.", call. = F)
  if (base::length(dirs) != 1) stop("Parameter dirs can contain only one path to a directory. Processing ends.", call. = F)
  if (!file.exists(dirs)) stop("Parameter dirs contain a directory that doesn't exist. Processing ends.", call. = F)
  if (base::regexpr("//", dirs) > 0) stop("Parameter dirs contain \"//\" instead of \"/\" or \"\\\\\".", call. = F)   # file.exists() doesn't catch path like dir//subdir (only one / should be used); potential issues with "\" is solved by R error message
  if (dirs %in% c("analysis", "code", "data", "output", "public")) {
    stop("Choose other than default workflowr directory. Processing ends.", call. = F)
  }

  # check input parameter subdirs
  if (!((subdirs == F || subdirs == T) && base::length(subdirs) == 1)) {
    stop("Input parameter subdirs can be only FALSE or TRUE. Processing ends.", call. = F)
  }

  # check if files with extension .Rmd or .rmd were chosen and continue only with patterns that meet this criteria
  #   workflowr::wflow_build() expects only files with extension Rmd or rmd otherwise following appears: Error: Invalid input for argument files,  Expected input: Only files with extension Rmd or rmd,  Observed input: ...
  if (!is.null(orig_rmd_patterns)) { # solving: orig_rmd_patterns == NULL
    for (pattern_num in 1:base::length(orig_rmd_patterns)) {
      if (!stringr::str_detect(orig_rmd_patterns[pattern_num], "(?i)^.*\\.[\\(, \\[]?\\s*r\\s*[\\,, \\|]?\\s*r?\\s*[\\), \\]]?md\\$?$")) {  # package "stringr" is used because it solves e.g. problems with escaping "]" that package function like "base::grepl()" has
        #   orig_rmd_patterns <- orig_rmd_patterns[-pattern_num]  # make this if () part better later - recalculate used regular expression to process at least those files that meet criteria (e.g. one pattern meets criteria, another one pattern doesn't meet criteria, so process the 1st pattern) - for this purpose to do this recalculation on this line needs to go to function create_orig_rmd_path()???
        #   if (base::length(orig_rmd_patterns) == 0) stop("No file meets criteria. Check parameter orig_rmd_patterns. Processing ends.")
        stop("Parameter orig_rmd_patterns doesn't point to file(s) with extension .Rmd or rmd. Processing ends.", call. = F)
      }
    } # for (pattern_num in 1:base::length(orig_rmd_patterns))
  }

  # give the option to delete teporary .Rmd files from "analysis"
  #   if there are temporary .Rmd files in "analysis" a message like following one may appears after trying to run function wflow_git_commit(): Error: Commit failed because no files were added. Attempted to commit the following files: (list of file paths) Any untracked files must manually specified even if `all = TRUE`.
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
      base::stop("You chose to stop rendering. Processing ends.", call. = F)
    } else (
      base::stop("You chose a not available option. Processing ends", call. = F)
    )
  }
}

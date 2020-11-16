#' @title
#' Check rules for directories and files
#' @description
#' Check rules for directories and .Rmd files to evaluate if rendering of .html files is possible.
#' If some check doesn't pass, processing ends.
#' This function is meant to be called only from function \code{\link{render_html}} so
#' it will be not exported therefore input variables have no default values.
#' @param dir_path
#' character
#' Path to a directory, under a main workflowr subdirectory, where original Rmd files are saved.
#' It can be only of length = 1.
#' Examples:
#' dir_path = "code-rmd"
#' dir_path = c("code-rmd/subdir1\\subdir2")
#' @param subdirs
#' logical
#' It can be only of length = 1.
#' If TRUE, file listing will also recurse into directories in parameter dir.
#' If FALSE, file listing will be only directly from directories in parameter dir.
#' @param patterns
#' character
#' Vector of paths or/and regular expression(s) to original .Rmd files.
#' If NULL, process all .Rmd files based values in parameters dir and subdirs.
#' If not NULL, process files matching written regular expression.
#' Examples:
#' patterns = "^.*page.*.\[  R , r \]md$")
#' patterns = c("page1.Rmd", ".*page2.*.Rmd")
#' @keywords workflowr, subdirectory
#' @return None but stop processing if no file meets criteria.
#' @examples
#' \dontrun{
#'   initial_checks(dir_path = "code-rmd", subdirs = T, patterns = ".*page.*.(R|r)md$")
#' }

initial_checks <- function(dir_path, subdirs, patterns) {
  # check input parameter "dir_path"
  if (base::is.null(dir_path))
    stop ("Parameter dir_path cannot be NULL. Processing ends.", call. = F)
  if (dir_path == "")
    stop("Parameter dir_path cannot be empty string. Processing ends.", call. = F)
  if (base::length(dir_path) != 1)
    stop("Parameter dir_path can contain only one path to a directory. Processing ends.", call. = F)
  if (!file.exists(dir_path))
    stop("Parameter dir_path contain a directory that doesn't exist. Processing ends.", call. = F)
  if (base::regexpr("//", dir_path) > 0)   # file.exists() doesn't catch path like dir//subdir (only one / should be used); potential issues with "\" is solved by R error message
    stop("Parameter dir_path contain \"//\" instead of \"/\" or \"\\\\\".", call. = F)
  if (dir_path %in% c("analysis", "code", "data", "output", "public")) {
    stop("Choose other than default workflowr directory. Processing ends.", call. = F)
  }

  # check input parameter "subdirs"
  if (!((subdirs == F || subdirs == T) && base::length(subdirs) == 1)) {
    stop("Input parameter subdirs can be only FALSE or TRUE. Processing ends.", call. = F)
  }

  # check input parameter "patterns"
  if (!is.null(patterns)) {
    ## if chosen files are set (not NULL) then check if all chosen files ends with .Rmd or .rmd
    ##   workflowr::wflow_build() expects only files with extension Rmd or rmd otherwise:
    ##     Error: ...  Expected input: Only files with extension Rmd or rmd,  Observed input: ...
    for (pattern_num in 1:base::length(patterns)) {
      if (!stringr::str_detect(  # package "stringr" solves some problems, e.g. with escaping "]", that functions like "base::grepl()" has
        patterns[pattern_num],
        "(?i)^.*\\.[\\(, \\[]?\\s*r\\s*[\\,, \\|]?\\s*r?\\s*[\\), \\]]?md\\$?$")) {
        ##   it can still happen that no file will exist but this is solved in create_orig_rmd_path
        stop("Not all chosen patterns point to file(s) with extension .Rmd or rmd. Processing ends.", call. = F)
      }
    }
  }

  # check an existence of unwanted temporary .Rmd files in directory "analysis"
  #   delete such files if chosen by an user
  #   they may occur if these files weren't removed at the end of generate_html() because of some fail
  #   if they are not deleted then calling function wflow_git_commit() in generate_html() ends with an error
  if (base::length(  # if some ".*--.*.Rmd" file exists in "analysis"
    temp_rmd_paths <- base::dir(
      path = "analysis", pattern = "(?i)^.*\\-\\-.*.rmd",
      full.names = T,    recursive = T
    )) > 0) {

    base::message(base::paste(
      "Following file names contain \"--\" and that's not allowed in directory \"analysis\":",
      base::paste(temp_rmd_paths, collapse = "\n"),
      "\nPlease choose one of the following options:",
      "1 - Delete listed files automatically and continue with rendering.",
      "2 - Stop rendering. I will manage relevant .Rmd files manually.",
      sep = "\n"
    ))

    option <- base::readline(prompt = "Choose 1 or 2: ")
    if (option == 1) {
      base::file.remove(temp_rmd_paths)
    } else if (option == 2) {
      base::stop("You chose to stop rendering. Processing ends.", call. = F)
    } else (
      base::stop("You chose a not available option. Processing ends", call. = F)
    )
  }
}

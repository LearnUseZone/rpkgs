#' @title
#' Stop process if rendering of .Rmd files into .html files is not possible
#' @description
#' Evaluate if rendering of .Rmd files into .html files is possible
#' by checking rules for directories and .Rmd files.
#' If some check doesn't pass, write a reason and stop processing.
#' This function is called only from function \code{\link{render_html}} therefore
#' it's not exported and its input variables have no default values.
#' @param dir_path
#' character of length = 1
#' Path to a subdirectory, under a main workflowr directory, where .Rmd files for rendering are saved.
#' A directory name can be first instead of using "./" etc. (e.g. "code-rmd" instead of "./code-rmd").
#' Examples:
#' dir_path = "code-rmd"
#' dir_path = c("code-rmd/subdir1\\subdir2")
#' @param subdirs
#' logical of length = 1
#' If TRUE, file listing will recurse into directories in parameter dir.
#' If FALSE, file listing will be only directly from directories in parameter dir.
#' @param patterns
#' character of length > 0 or NULL
#' A character vector of paths to .Rmd files for rendering.
#' If NULL, process all .Rmd files based values in parameters dir and subdirs.
#' If not NULL, process files matching a regular expression.
#' Examples:
#' patterns = "^.*page.*.\[  R , r \]md$")
#' patterns = c("page1.Rmd", ".*page2.*.Rmd")
#' @keywords workflowr, subdirectory
#' @return FALSE if some check fails, TRUE if all checks were OK,
#' a logical vector only with TRUE values of length equals to a number of deleted files from directory "analysis"
#' @examples
#' \dontrun{
#'   initial_checks(dir_path = "code-rmd", subdirs = T, patterns = ".*page.*.(R|r)md$")
#' }

initial_checks <- function(dir_path, subdirs, patterns) {
  # check input parameter "dir_path"
  if (base::is.null(dir_path))
    stop("Parameter 'dir_path' cannot be NULL.", call. = F)
  if (dir_path == "")
    stop("Parameter 'dir_path' cannot be empty string.", call. = F)
  if (base::length(dir_path) != 1)
    stop("Parameter 'dir_path' can contain only 1 path to a directory.", call. = F)
  if (!file.exists(dir_path))
    stop("Parameter 'dir_path' contains a directory that doesn't exist.", call. = F)
  if (base::regexpr("//", dir_path) > 0)  # file.exists() doesn't catch path like dir//subdir, dir///subdir, dir////subdir, etc. (only one "/" has to be used); potential issues with "\" is solved by R error message
    stop("Parameter 'dir_path' contains \"//\" instead of \"/\" or \"\\\\\".", call. = F)
  if (dir_path %in% c("analysis", "code", "data", "docs", "output", "public"))
    stop("Parameter 'dir_path' cannot be a default workflowr directory.", call. = F)

  # check input parameter "subdirs"
  if (!((subdirs %in% c(F, T)) && base::length(subdirs) == 1))
    stop("Parameter 'subdirs' can be only FALSE or TRUE.", call. = F)

  # check input parameter "patterns"
  if (!is.null(patterns)) {
    ## if chosen files are set (not NULL) then check if all chosen files ends with .Rmd or .rmd
    ##   workflowr::wflow_build() expects only files with extension Rmd or rmd otherwise:
    ##     Error: ...  Expected input: Only files with extension Rmd or rmd,  Observed input: ...
    for (pattern_num in 1:base::length(patterns)) {
      if (!stringr::str_detect(  # package "stringr" solves some problems, e.g. with escaping "]", that functions like "base::grepl()" has
        patterns[pattern_num],
        "(?i)^.*\\.[\\(, \\[]?\\s*r\\s*[\\,, \\|]?\\s*r?\\s*[\\), \\]]?md\\$?$"))
        ##   it can still happen that no file will exist but this is solved in create_rmd_paths()
        stop("Parameter 'patterns' has to point only to files with extension .Rmd or .rmd.", call. = F)
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
    )) > 0) {  # if this part doesn't run then initial_checks() returns NULL

    base::message(
      "Following file names contain \"--\" which isn't allowed:", "\n",  # I simply prefer "\n" between commas (if possible)
      base::paste(temp_rmd_paths, collapse = "\n"), "\n\n",
      "Choose 'y' or 'Y' to automatically delete listed files and continue with processing.", "\n",
      "Choose anything else to stop process and manage relevant files manually."
    )

    choice <- base::readline(prompt = "My choice: ")

    if (choice %in% c("y", "Y")) {
      base::file.remove(temp_rmd_paths)  # if this part run then initial_checks() returns a number of TRUE equals to number of deleted files
    } else {  # this is not an error it's simply message about a user's choice
      base::message("You chose to stop rendering.")
      return(F)  # if this part run then initial_checks() returns FALSE (length = 1)
    }
  }
}

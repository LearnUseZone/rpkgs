#' @title Check if rendering .html files from .Rmd files from subdirectories is possible
#' @description
#' Check if rendering .html files from .Rmd files from subdirectories is possible
#' @param dir
#' character (default: "code-Rmd").
#' Path to a directory, under a main workflowr subdirectory, where original Rmd files are saved.
#' @param only_subdirs
#' I will add a description later.
#' @param path_orig_Rmd
#' character (default: NULL).
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

initial_checks <- function(dir = "code-Rmd", only_subdirs = NULL, path_orig_Rmd = NULL) {
  # check of definition and existence of main directory
  if (base::length(dir) > 1) stop(base::paste0("Only one directory can be defined in parameter dir."))
  if (!base::file.exists(dir)) stop(base::paste0("Directory ", dir, " doesn't exist in main workflowr directory."))
  if (dir %in% c("analysis", "code", "data", "output", "public")) {
    stop(base::paste0("Choose other than default workflowr directory."))
  }

  # check of existence of subdirectories
  if (!base::is.null(only_subdirs)) {
    only_subdirs <- base::as.matrix(base::file.path("code-Rmd", only_subdirs))
    if (!base::all(base::apply(only_subdirs, 2, base::file.exists))) {
      stop("At least one of subdirectories doesn't exist.")
    }
  }

  # check existence of files defined manually and without using regular expression
  if (!base::is.null(path_orig_Rmd)) {  # don't use "&" with following "if" otherwise error "argument is of length zero" arises
    if (base::substr(path_orig_Rmd, nchar(path_orig_Rmd) - 7, nchar(path_orig_Rmd)) != ".(R|r)md") {
      path_orig_Rmd <- base::gsub("\\\\", "/", path_orig_Rmd)  # see explanation in "Notes" at the end of this function
      for (iteration_path_Rmd in path_orig_Rmd) {  # is any apply function better then this for()???
        dir_path_Rmd <- base::file.path(dir, iteration_path_Rmd)
        if (!base::file.exists(dir_path_Rmd)) stop(base::paste0("File doesn't exist: ", dir_path_Rmd))      # if a file doesn't exist a message is written and code stops
      }
    }
  }  # if (!base::is.null(path_orig_Rmd))
}

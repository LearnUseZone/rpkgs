#' @title Check if rendering .html files from .Rmd files from subdirectories is possible
#' @description
#' Check if rendering .html files from .Rmd files from subdirectories is possible
#' @param dir
#' character (default: "code-Rmd").
#' Path to a directory, under a main workflowr subdirectory, where original Rmd files are saved.
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

initial_checks <- function(dir = "code-Rmd", path_orig_Rmd = NULL) {
  if (base::length(dir) > 1) stop(base::paste0("Only one directory can be defined in parameter dir."))
  if (!base::file.exists(dir)) stop(base::paste0("Directory ", dir, " doesn't exist in main workflowr directory."))

  # check existence of files manually specified in variable "files"
  if (!base::is.null(path_orig_Rmd)) {
    path_orig_Rmd <- base::gsub("\\\\", "/", path_orig_Rmd)  # see explanation in "Notes" at the end of this function
    for (iteration_path_Rmd in path_orig_Rmd) {
      dir_path_Rmd <- base::file.path(dir, iteration_path_Rmd)
      if (!base::file.exists(dir_path_Rmd)) stop(base::paste0("File doesn't exist: ", dir_path_Rmd))      # if a file doesn't exist a message is written and code stops
    }
  }
}

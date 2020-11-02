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

initial_checks <- function(dir = "code-Rmd", only_subdirs = NULL, path_orig_Rmd = NULL) {  # function will be splitted and a new function will be named initial_preparations()???
  real_path_orig_Rmd <- c()  # initializing a return variable for a list of found .Rmd files

  # check of definition and existence of main directory #
  if (base::length(dir) > 1) stop(base::paste0("Only one directory can be defined in parameter dir."))
  if (!base::file.exists(dir)) stop(base::paste0("Directory ", dir, " doesn't exist in main workflowr directory."))
  if (dir %in% c("analysis", "code", "data", "output", "public")) {
    stop(base::paste0("Choose other than default workflowr directory."))
  }

  # check of existence of subdirectories #
  if (!base::is.null(only_subdirs)) {
    only_subdirs <- base::as.matrix(base::file.path("code-Rmd", only_subdirs))
    if (!base::all(base::apply(only_subdirs, 2, base::file.exists))) {
      stop("At least one of subdirectories doesn't exist.")
    }
  }

  # start - generate a vector of real paths (variable real_path_orig_Rmd) to path_orig_Rmd #
  #   only if path_orig_Rmd contains "$" at the and of string(s) it's processed as a regular expression
  if (!base::is.null(path_orig_Rmd)) {
    path_orig_Rmd <- base::gsub("\\\\", "/", path_orig_Rmd)  # ensure that temporary .Rmd file will contain "/" instead of "\\" in a path to a relevant temporary (knitr) .Rmd file defined in r chunk code created in function base::cat() used in function generate_rmd()
    if (!base::is.null(only_subdirs)) only_subdirs <- base::gsub("\\\\", "/", only_subdirs)

    if (!base::is.null(only_subdirs)) {  # if only_subdirs contains at least one subdirectory name
      for (iteration_path_Rmd in path_orig_Rmd) {  # is any function from "apply" family better than this for()???; in this for() I have to solve a situation where more only_subdirs are specified and path_orig_Rmd is not a regular expression???
        dir_path_Rmd <- base::file.path(only_subdirs, iteration_path_Rmd)
      }
      lf_recursive <- F                  # listing will not recurse into directories; lf = abbreviation for list_files
      lf_dir <- only_subdirs
      iteration_steps = length(only_subdirs)
    } else {  # if only_subdirs = NULL
      for (iteration_path_Rmd in path_orig_Rmd) {  # is any function from "apply" family better than this for()???; in this for() I have to solve a situation where more only_subdirs are specified and path_orig_Rmd is not a regular expression???
        dir_path_Rmd <- base::file.path(dir, iteration_path_Rmd)
      }
      lf_recursive <- T
      lf_dir <- dir
      iteration_steps = 1
    }

    if (!stringr::str_detect( # if path_orig_Rmd is not a regular expression pointing to .Rmd (all letters are case insensitive) file(s)
      path_orig_Rmd,      #   stringr package is needed because if regular expressions with special characters is used then functions like grep() may fail (e.g. when escaping "]")
      "(?i)^.*\\.[\\(, \\[]?\\s*r\\s*[\\,, \\|]?\\s*r?\\s*[\\), \\]]?md\\$$")) {
      for (iteration in 1:base::length(dir_path_Rmd)) { # only_subdirs can contain more strings while path_orig_Rmd is not a regular expression
        if (!base::file.exists(dir_path_Rmd[iteration])) stop(base::paste0("File ", dir_path_Rmd[iteration], " doesn't exist or regular expression without $ as the last character is used or a regular expression doesn't point to file with extension .Rmd."))
        # if only_subdirs = NULL and path_orig_Rmd is not a regular expression then nothing will be generated even if some of files exist - update this behavior???
      }
    } else {  # if path_orig_Rmd is a regular expression pointing to .Rmd (all letters are case insensitive) file(s)
      num_of_existing_files = 0  # initialize a new variable
      for (iteration in 1:iteration_steps) {
        try (   # result_of_try <- try (...) is not needed because result_of_try would be the same as result_path_orig_Rmd in this case
          { result_path_orig_Rmd <- mapply(
            base::list.files,
            path = lf_dir[iteration],
            full.names = T,      # example of a full name: code-Rmd/subPages/test.Rmd; file.path() in many other code parts is not needed anymore
            recursive = lf_recursive,
            pattern = path_orig_Rmd
          )
          }
        )
        if (class(result_path_orig_Rmd) == "character" || class(result_path_orig_Rmd) == "matrix") {
          num_of_existing_files = num_of_existing_files + 1
          real_path_orig_Rmd <- base::append(real_path_orig_Rmd, result_path_orig_Rmd)
        }
      } # end - for (iteration in 1:base::length(only_subdirs))
      if (length(real_path_orig_Rmd) == 0) stop("No file meet criteria.")

      ##print(real_path_orig_Rmd)
    }
  }  # if (!base::is.null(path_orig_Rmd))
  # end - generate a vector of real paths (variable real_path_orig_Rmd) to path_orig_Rmd #
  return(real_path_orig_Rmd)
}

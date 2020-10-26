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

  # check existence of files defined manually and without using regular expression #
  #   - One "if" (using "&") causes error "argument is of length zero" if path_orig_Rmd is NULL.
  if (!base::is.null(path_orig_Rmd)) {
    if (!base::all(  # following regular expression define that .Rmd (case insensitive) has to be defined in path_orig_Rmd
      stringr::str_detect(path_orig_Rmd, "(?i)^.*\\.[\\(, \\[]?\\s*r\\s*[\\,, \\|]?\\s*r?\\s*[\\), \\]]?md\\$$")  # stringr package is needed because if regular expressions with special characters is used then functions like grep() may fail (e.g. when excaping "]")
    )) { # if path_orig_Rmd is not regular expression ($ is required as the last character to determine that it's meant to be a regular expression)
      path_orig_Rmd <- base::gsub("\\\\", "/", path_orig_Rmd)  # see explanation in "Notes" at the end of this function
      for (iteration_path_Rmd in path_orig_Rmd) {              # is any apply function better then this for()???; in this for() I have to solve a situation where more only_subdirs are specified and path_orig_Rmd is not a regular expression???
        if (is.null(only_subdirs)) {
          dir_path_Rmd <- base::file.path(dir, iteration_path_Rmd)
        } else {
          dir_path_Rmd <- base::file.path(only_subdirs, iteration_path_Rmd)
        }
        for (iteration in 1:base::length(dir_path_Rmd)) { # only_subdirs can contain more strings while path_orig_Rmd is not a regular expression
          if (!base::file.exists(dir_path_Rmd[iteration])) stop(base::paste0("File ", dir_path_Rmd[iteration], " doesn't exist or regular expression without $ as the last character is used or a regular expression doesn't point to file with extension .Rmd."))
        }
      }

    } else {  # this part is for situation where only_subdirs is/isn't defined and path_orig_Rmd is set to regular expression but no such file is found in specified only_subdirs, e.g. generate_html(dir = "code-Rmd", only_subdirs = "eToro1", path_orig_Rmd = "testToDelete.Rmd$"); if path_orig_Rmd isn't specified as a regular expression than this situation is solved in if (!base::all(base::grepl("(.*R\\|?r?\\)?md\\$$|.*r\\|?R?\\)?md\\$$)", path_orig_Rmd)))
      path_orig_Rmd <- base::gsub("\\\\", "/", path_orig_Rmd)  # see explanation in "Notes" at the end of this function
      path_orig_Rmd <- base::gsub("\\$$", "", path_orig_Rmd)   # remove last $ (this is not in previous part (but still it could be optimized)???

      # find out if at least one file exist (as a combination of only_subdirs and path_orig_Rmd)
      result_of_try <- try(
        {
          if (!base::is.null(only_subdirs)) {       # if only_subdirs contains at least one subdirectory name
            lf_recursive <- F                       # listing will not recurse into directories
            # lf_dir <- file.path(dir, only_subdirs)  # lf = abbreviation for list_files
            lf_dir <- only_subdirs
          } else {
            lf_recursive <- T
            lf_dir <- dir
          }
          path_orig_Rmd <- mapply(
            base::list.files,
            path = lf_dir,
            full.names = T,      # example of a full name: code-Rmd/subPages/test.Rmd; file.path() in many other code parts is not needed anymore
            recursive = lf_recursive,
            pattern = path_orig_Rmd
          )
        }
      )
      if (class(result_of_try) != "character") stop("Some of files defined in path_orig_Rmd doesn't exist in directories defined in only_subdirs.")

      # print(111)
      # print(result_of_try)

    }
  }  # if (!base::is.null(path_orig_Rmd))
}

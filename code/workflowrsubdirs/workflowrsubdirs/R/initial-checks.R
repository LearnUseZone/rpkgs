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
  real_path_orig_Rmd <- c()  # initializing a return variable for a list of found .Rmd files, because append() is used in some cases

  # check of definition and existence of main directory
  if (base::length(dir) > 1) stop(base::paste0("Only one directory can be defined in parameter dir."))
  if (!base::file.exists(dir)) stop(base::paste0("Directory ", dir, " doesn't exist in main workflowr directory."))
  if (dir %in% c("analysis", "code", "data", "output", "public")) {
    stop(base::paste0("Choose other than default workflowr directory."))
  }

  # initial settings based on only_subdirs (check of existence of subdirectories)
  ## solving: only_subdirs != NULL
  if (!base::is.null(only_subdirs)) {  # if only_subdirs contains at least one subdirectory name
    only_subdirs <- base::gsub("\\\\", "/", only_subdirs)
    lf_dir <- base::as.matrix(base::file.path(dir, only_subdirs))  # for mapply() below
    lf_recursive <- F  # listing will not recurse into directories; lf_recursive is set only based on only_subdirs
    subdirs_count = base::length(only_subdirs)
    ## solving: only_subdirs == NULL
  } else {
    lf_dir <- dir
    lf_recursive <- T  # listing will recurse into directories; lf = abbreviation for list_files
    subdirs_count = 1
  }

  # solving: path_orig_Rmd == NULL
  #   following if is not needed anymore: if (!stringr::str_detect(path_orig_Rmd[iteration_path_Rmd], "(?i)^.*\\.[\\(, \\[]?\\s*r\\s*[\\,, \\|]?\\s*r?\\s*[\\), \\]]?md\\$$"))
  if (base::is.null(path_orig_Rmd)) path_orig_Rmd = "^.*\\.(R|r)md$"  # if path_orig_Rmd = NULL then search for all files in set directory (dir or only_subdirs)


  # generate a character vector of .Rmd files for further rendering
  num_of_existing_files <- 0  # initialize a new variable

  ## create a character vector or a list of visible files #
  for (iii in 1:subdirs_count) {
    result_path_orig_Rmd <- try ({ # result_of_try <- try (...) would be the same as result_path_orig_Rmd in this case; it's better to have result_path_orig_Rmd <- try, in case that something in mapply() fails
      base::mapply(
        base::list.files,    # if some file doesn't exist then list.files() produces list (instead of a character vector)
        path = lf_dir[iii],  # path consisting of directories and subdirectories
        full.names = T,      # example of a full name: code-Rmd/subPages/test.Rmd; file.path() in many other code parts is not needed anymore
        recursive = lf_recursive,
        pattern = path_orig_Rmd
        # Notes
        #   include.dirs = T # include a subdirectory that matches a regular expression in path_orig_Rmd
        #   all.files = F    # only visible files are processed
      )
    })
    result_path_orig_Rmd <- base::unname(base::unlist(result_path_orig_Rmd))     # remove unwanted list elements if some of path_orig_Rmd doesn't exist; also if result_path_orig_Rmd is a matrix, size will be lowered
    real_path_orig_Rmd <- base::append(real_path_orig_Rmd, result_path_orig_Rmd) # each for cycle may generate new result_path_orig_Rmd
  }
  if (length(real_path_orig_Rmd) == 0) stop("No file meet criteria.")   # move to generate_html???

  return(real_path_orig_Rmd)  # return a vector of real paths
}

#' @title Create paths to original .Rmd files for future rendering
#' @description
#' Check if rendering .html files from .Rmd files from subdirectories is possible and
#' in such case, create paths to original .Rmd files for future rendering otherwise stop processing.
#' @param dir
#' character (default: "code-Rmd").
#' Path to a directory, under a main workflowr subdirectory, where original Rmd files are saved.
#' @param only_subdirs
#' character (default: NULL). It's case insensitive.
#' If only_subdirs == NULL then all subdirectories and files within directory in input parameter dir are processed, otherwise only files in subdirectories in this input parameter only_subdirs are processed.
#' If only_subdirs != NULL then it's a vector of subdirectories in directory specified in input parameter dir.
#' Examples: only_subdirs = NULL; only_subdirs == c("subdir1", "subdir.Rmd")
#' @param orig_Rmd_pattern
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

create_orig_Rmd_path <- function(dir = "code-Rmd", only_subdirs = NULL, orig_Rmd_pattern = NULL) {

  # check of definition and existence of a user chosen directories
  if (base::length(dir) > 1) stop(base::paste0("Only one directory can be defined in parameter dir."))  # this solution should work even if "dir" contains more directories or paths to subdirectories - check it and adjust solution if needed???
  if (!base::file.exists(dir)) stop(base::paste0("Directory ", dir, " doesn't exist in main workflowr directory."))
  if (dir %in% c("analysis", "code", "data", "output", "public")) {
    stop(base::paste0("Choose other than default workflowr directory."))
  }

  # check .Rmd files in directories analysis and input variable dir
  #   ensure that there are no temporary .Rmd files in directory "analysis" otherwise you may receive message like following one after trying to run function wflow_git_commit(...): Error: Commit failed because no files were added. Attempted to commit the following files: (list of file paths) Any untracked files must manually specified even if `all = TRUE`.
  if (base::length(
    double_hyphen_paths <- base::dir(
      path = c("analysis", dir),
      pattern = "(?i)^.*\\-\\-.*.rmd",
      full.names = T,
      recursive = T
    )) > 0) {  # if some "*--*.Rmd" file was found

    base::cat(
      "Following .Rmd files contain \"--\" which is not allowed in written directories:\n",
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


  # Put following to a function create_orig_Rmd_path()???
  # variable initialization
  orig_Rmd_path <- c()  # return variable for created original .Rmd paths; initialization required because of append() below

  # initial settings based on only_subdirs for mapply() below
  ## solving: only_subdirs != NULL
  if (!base::is.null(only_subdirs)) {
    only_subdirs <- base::gsub("\\\\", "/", only_subdirs)
    lf_dir <- base::as.matrix(base::file.path(dir, only_subdirs)) # lf = list_files
    lf_recursive <- F  # listing will not recurse into directories
    subdirs_count = base::length(only_subdirs)
    ## solving: only_subdirs == NULL
  } else {
    lf_dir <- dir
    lf_recursive <- T  # listing will recurse into directories
    subdirs_count = 1
  }

  # initial settings based on orig_Rmd_pattern for mapply() below
  ## solving: orig_Rmd_pattern == NULL
  if (base::is.null(orig_Rmd_pattern)) orig_Rmd_pattern = "(?i)^.*\\.rmd$"  # if orig_Rmd_pattern = NULL then search for all files in set directory (dir or only_subdirs)


  # generate a character vector of .Rmd files for further rendering
  ## create a character vector or a list of visible files #
  for (iii in 1:subdirs_count) {
    result_orig_Rmd_pattern <- try ({ # result_of_try <- try (...) would be the same as result_orig_Rmd_pattern in this case; it's better to have result_orig_Rmd_pattern <- try, in case that something in mapply() fails
      base::mapply(
        base::list.files,    # if some file doesn't exist then list.files() produces list (instead of a character vector)
        path = lf_dir[iii],  # path consisting of directories and subdirectories
        full.names = T,      # example of a full name: code-Rmd/subPages/test.Rmd
        recursive = lf_recursive,
        pattern = orig_Rmd_pattern
        # Notes
        #   include.dirs = T # include a subdirectory that matches a regular expression in orig_Rmd_pattern
        #   all.files = F    # only visible files are processed
      )
    })
    result_orig_Rmd_pattern <- base::unname(base::unlist(result_orig_Rmd_pattern))        # remove unwanted list elements if some of orig_Rmd_pattern doesn't exist; also if result_orig_Rmd_pattern is a matrix, size will be lowered
    orig_Rmd_path <- base::append(orig_Rmd_path, result_orig_Rmd_pattern) # append() - because each for () cycle may generate new result_orig_Rmd_pattern
  }
  if (length(orig_Rmd_path) == 0) stop("Processing ends because no file meets criteria.")
  return(orig_Rmd_path)  # return a vector of real paths

  # Notes
  #   If there's more complex regular expression like the following one, use package "stringr" because it solves e.g. problems with escaping "]" that package function like "grepl()" has.
  #     if (!stringr::str_detect(orig_Rmd_pattern[iteration_path_Rmd], "(?i)^.*\\.[\\(, \\[]?\\s*r\\s*[\\,, \\|]?\\s*r?\\s*[\\), \\]]?md\\$$"))
}

#' @title Render .html files from their original .Rmd files stored in subdirectories
#' @description
#' Similarly as for workflowr package, when specifying path to directories or files in function generate_html(), you can start by typing directory name, so for example, you can write "analysis" instead of "./analysis".
#' Processed steps:
#' 1. Check existence of .Rmd file in a chosen subdirectory.
#' - If it doesn't exist, inform about it and stop processing.
#' 2. Prepare temporary file names by substitution  of "/" in file path by "&#8208;&#8208;".
#' 3. Function generate_rmd() is used to generate temporary .Rmd files from their original .Rmd files that are saved in subdirectories and save them into directory "analysis".
#' 4. If commit = TRUE, create a commit of these temporary .Rmd files with text "separate commit of temporary .Rmd files".
#' 5. Generate .html files from temporary .Rmd files.
#'    - So the resulting .html files are associated with their original .Rmd files (temporary .Rmd files are simply product of a helping step).
#' 6. Delete temporary .Rmd files from directory "analysis".
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
#' @param commit
#' character (default: FALSE).
#' commit = TRUE creates a separate commit of temporary .Rmd files (temporary saved in directory "analysis").
#' Suggestion: Use commit = TRUE only after your original .Rmd files saved in subdirectories are tested properly and so are completely ready, otherwise you could have pointlessly many commits.
#' @keywords workflowr, subdirectory
#' @return Final .html file from its original .Rmd file saved in a subdirectory.
#' @export generate_html
#' @examples
#' \dontrun{
#'   generate_html()
#'   generate_html("code-Rmd", c("subPages1/testPrint1.Rmd", "subPages2/testPrint2.Rmd"), T)
#'   generate_html("code-Rmd", c("subPages1\\testPrint1.Rmd", "subPages2\\testPrint2.Rmd"), F)
#' }

generate_html <- function(dir = "code-Rmd", only_subdirs = NULL, path_orig_Rmd = NULL, commit = F) {
  # only_subdirs can contain also text .Rmd (write to README.Rmd)???
  # initial settings #
  base::setwd(here::here())  # set .Rproj (workflowr) project directory as a working directory (in case it was changed after opening .Rproj file)
  real_path_orig_Rmd <- initial_checks(dir, only_subdirs, path_orig_Rmd)  # it's after setwd(here()) to ensure that checks start in main workflowr directory

  # initial settings for NULL values #
  if (base::is.null(path_orig_Rmd)) path_orig_Rmd = "^.*\\.(R|r)md$"
  if (!base::is.null(only_subdirs)) {       # if only_subdirs contains at least one subdirectory name
    lf_recursive <- F                       # listing will not recurse into directories
    lf_dir <- file.path(dir, only_subdirs)  # lf = abbreviation for list_files
  } else {
    lf_recursive <- T
    lf_dir <- dir
  }

  # create a list of visible files #
  if (length(real_path_orig_Rmd) > 0) {
    slash_location <- stringi::stri_locate_last(real_path_orig_Rmd, fixed = "/")  # look for another way than using stringi???
    path_orig_Rmd <- mapply(
      base::list.files,
      path = stringr::str_sub(real_path_orig_Rmd, 1, slash_location[,1] - 1),
      full.names = T,      # example of a full name: code-Rmd/subPages/test.Rmd; file.path() in many other code parts is not needed anymore
      recursive = lf_recursive,
      pattern = stringr::str_sub(real_path_orig_Rmd, slash_location[,1] + 1, nchar(real_path_orig_Rmd))

      # Notes
      #   include.dirs = T would mean that if a subdirectory matches a regular expression, then this subdirectory is included in path_orig_Rmd
      #   all.files = FALSE means that only visible files are processed
    )
  } else {
    # create a list of visible files #
    path_orig_Rmd <- mapply(
      base::list.files,
      path = lf_dir,
      full.names = T,      # example of a full name: code-Rmd/subPages/test.Rmd; file.path() in many other code parts is not needed anymore
      recursive = lf_recursive,
      pattern = path_orig_Rmd
      # all.files = F      # only visible files are processed
      # include.dirs = T   # include a subdirectory that matches a regular expression in path_orig_Rmd
    )
  }

  ### # for tests
  ### print(path_orig_Rmd)
  ### print(length(path_orig_Rmd))

  path_knitr_Rmd <- sub("\\.Rmd$", "_knitr.Rmd", path_orig_Rmd)   # work with path_knitr_Rmd has to be after check of existence of path_orig_Rmd
  base::mapply(knitr::knit, path_orig_Rmd, path_knitr_Rmd)        # render path_orig_Rmd to path_knitr_Rmd in order to get correctly "calculated" inline R code in YAML header; find out if (maybe it's not possible at all or it's not worth it) is it possible to use knitr to get only YAML header and then join it with the rest of .Rmd code (let's start with https://stackoverflow.com/questions/39885363/importing-common-yaml-in-rstudio-knitr-document)???

  temp_name_Rmd <- base::gsub("/", "--", path_orig_Rmd)           # change "/" in paths to .Rmd files to generate file names (not paths) with "--", these are new file names of .Rmd files that will be generated in directory "analysis"
  analysis_Rmd <- base::file.path("analysis", temp_name_Rmd)      # paths to temporary .Rmd files that will be also deleted after .html files are rendered from them
  base::file.remove(base::file.path("analysis", dir(path = "analysis", pattern = "^.*\\-\\-.*.Rmd")))  # ensure that there are no temporary .Rmd files in directory "analysis" otherwise you may receive message like following one after trying to run function wflow_git_commit(...): Error: Commit failed because no files were added. Attempted to commit the following files: (list of file paths) Any untracked files must manually specified even if `all = TRUE`.

  base::mapply(generate_rmd, dir, path_knitr_Rmd, temp_name_Rmd)  # generate temporary .Rmd files
  if (commit == T) {
    workflowr::wflow_git_commit("analysis/*--*Rmd", "separate commit of temporary .Rmd files", all = T)
  }
  workflowr::wflow_build(analysis_Rmd)  # generate .html files from temporary .Rmd files
  base::file.remove(analysis_Rmd)       # delete temporary .Rmd files from directory "analysis"
  base::file.remove(path_knitr_Rmd)     # delete file created using knitr::knit(); I will look at this file.path() and also other file.path() in this function generate_html() and also generate_rmd() and try to simplify them (delete uneccessary parts)???

}

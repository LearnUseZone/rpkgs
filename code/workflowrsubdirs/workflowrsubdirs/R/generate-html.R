#' @title
#' Render .html files from their original .Rmd files stored in subdirectories
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
#' character (default: NULL). It's case insensitive.
#' If only_subdirs == NULL then all subdirectories and files within directory in input parameter dir are processed, otherwise only files in subdirectories in this input parameter only_subdirs are processed.
#' If only_subdirs != NULL then it's a vector of subdirectories in directory specified in input parameter dir.
#' Examples: only_subdirs = NULL; only_subdirs == c("subdir1", "subdir.Rmd")
#' @param orig_rmd_pattern
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

generate_html <- function(dir = "code-Rmd", only_subdirs = NULL, orig_rmd_pattern = NULL, commit = F) {
  # initial settings
  base::setwd(here::here())  # setting of project (.Rproj) directory as a working directory in case it was changed after opening .Rproj file; it's necessary to have some of following steps after this setting
  initial_checks(dir, only_subdirs, orig_rmd_pattern)  # has the "power" to stop following processing
  orig_rmd_path <- create_orig_rmd_path(dir, only_subdirs, orig_rmd_pattern) # get path to original .Rmd files for future rendering

  path_knitr_Rmd <- base::sub("\\.Rmd$", "_knitr.Rmd", orig_rmd_path) # work with path_knitr_Rmd has to be after check of existence of orig_rmd_path
  base::mapply(knitr::knit, orig_rmd_path, path_knitr_Rmd)            # render orig_rmd_path to path_knitr_Rmd in order to get correctly "calculated" inline R code in YAML header; find out if (maybe it's not possible at all or it's not worth it) is it possible to use knitr to get only YAML header and then join it with the rest of .Rmd code (let's start with https://stackoverflow.com/questions/39885363/importing-common-yaml-in-rstudio-knitr-document)???

  temp_rmd_name <- base::gsub("/", "--", orig_rmd_path)       # change "/" in paths to .Rmd files to generate file names (not paths) with "--", these are new file names of .Rmd files that will be generated in directory "analysis"
  temp_rmd_path <- base::file.path("analysis", temp_rmd_name) # paths to temporary .Rmd files that will be also deleted after .html files are rendered from them

  base::mapply(generate_rmd, dir, path_knitr_Rmd, temp_rmd_name)  # generate temporary .Rmd files
  if (commit == T) {
    workflowr::wflow_git_commit("analysis/*--*Rmd", "separate commit of temporary .Rmd files", all = T)
  }
  workflowr::wflow_build(temp_rmd_path)  # render .html files from temporary .Rmd files

  # delete temporary helping .Rmd files
  base::file.remove(temp_rmd_path)
  base::file.remove(path_knitr_Rmd)
}

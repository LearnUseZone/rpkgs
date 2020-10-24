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

generate_html <- function(dir = "code-Rmd", path_orig_Rmd = NULL, commit = F) {
  base::setwd(here::here())           # set .Rproj (workflowr) project directory as a working directory (in case it was changed after opening .Rproj file)
  initial_checks(dir, path_orig_Rmd)  # has to be after base::setwd(here::here()) in order to be sure that checks start in main workflowr directory

  # create a list of files
  if (base::is.null(path_orig_Rmd)) {
    path_orig_Rmd <- base::list.files(  # generate paths (not only file names) to .Rmd files in subdirectories under directory in parameter "dir"
      dir,
      recursive = T,
      include.dirs = T,
      pattern = "./*.(r|R)md"
    )
  }

  path_knitr_Rmd <- sub(".Rmd", "_knitr.Rmd", path_orig_Rmd)     # work with path_knitr_Rmd has to be after check of existence of path_orig_Rmd
  base::mapply(knitr::knit, base::file.path(dir, path_orig_Rmd), base::file.path(dir, path_knitr_Rmd))  # render path_orig_Rmd to path_knitr_Rmd in order to get correctly "calculated" inline R code in YAML header; find out if (maybe it's not possible at all or it's not worth it) is it possible to use knitr to get only YAML header and then join it with the rest of .Rmd code (let's start with https://stackoverflow.com/questions/39885363/importing-common-yaml-in-rstudio-knitr-document)???

  temp_name_Rmd <- base::gsub("/", "--", path_orig_Rmd)  # change "/" in paths to .Rmd files to generate file names (not paths) with "--", these are new file names of .Rmd files that will be generated in directory "analysis"
  analysis_Rmd <- base::file.path("analysis", temp_name_Rmd)    # paths to temporary .Rmd files that will be also deleted after .html files are rendered from them
  base::file.remove(base::file.path("analysis", dir(path = "analysis", pattern = ".*\\-\\-.*.Rmd")))  # ensure that there are no temporary .Rmd files in directory "analysis" otherwise you may receive message like following one after trying to run function wflow_git_commit(...): Error: Commit failed because no files were added. Attempted to commit the following files: (list of file paths) Any untracked files must manually specified even if `all = TRUE`.

  base::mapply(generate_rmd, dir, path_knitr_Rmd, temp_name_Rmd)       # generate temporary .Rmd files
  if (commit == T) {
    workflowr::wflow_git_commit("analysis/*--*Rmd", "separate commit of temporary .Rmd files", all = T)
  }
  workflowr::wflow_build(analysis_Rmd)  # generate .html files from temporary .Rmd files
  base::file.remove(analysis_Rmd)       # delete temporary .Rmd files from directory "analysis"
  base::file.remove(base::file.path(dir, path_knitr_Rmd))  # delete file created using knitr::knit(); I will look at this file.path() and also other file.path() in this function generate_html() and also generate_rmd() and try to simplify them (delete uneccessary parts)???

# Notes
# Explanation for usage of (Option 1): path_orig_Rmd <- base::gsub("\\\\", "/", path_orig_Rmd)
#   Option 2 would be to add to generate_rmd() following code:
#     rel_path <- sub("\\\\", "\\\\\\\\", rel_path) # "\\\\\\\\" will be written as "\\" in generated .Rmd file, if I keep only "\\\\" then there's only "\" in generated .Rmd file which wouldn't work correctly
#   plus in generate_html(), replace temp_name_Rmd <- base::gsub("/", "--", path_orig_Rmd) for
#     temp_name_Rmd <- base::gsub("\\\\", "--", base::gsub("/", "--", path_orig_Rmd))
#   Goal of both options is to ensure that temporary .Rmd file will contain "/" or "\\" in path to a relevant temporary (knitr) .Rmd file defined in r chunk code created in function base::cat() used in function generate_rmd().
}

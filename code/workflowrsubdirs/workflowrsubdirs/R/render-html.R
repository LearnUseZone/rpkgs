#' @title
#' Render .html files from their original .Rmd files stored in subdirectories
#' @description
#' Similarly as for workflowr package, when specifying path to directories or files in function
#' this function render_html(), you can start by typing directory name,
#' so for example, you can write "analysis" instead of "./analysis".
#' Processed steps:
#' 1. Check existence of .Rmd file in a chosen subdirectory.
#' - If it doesn't exist, inform about it and stop processing.
#' 2. Prepare temporary file names by substitution  of "/" in file path by "&#8208;&#8208;".
#' 3. Function generate_rmd() is used to generate temporary .Rmd files from their original
#' .Rmd files that are saved in subdirectories and save them into directory "analysis".
#' 4. If commit = TRUE, create a commit of these temporary .Rmd files with text
#' "separate commit of temporary .Rmd files".
#' 5. Generate .html files from temporary .Rmd files.
#'    - So the resulting .html files are associated with their original .Rmd files
#'    (temporary .Rmd files are simply product of a helping step).
#' 6. Delete temporary .Rmd files from directory "analysis".
#' @param dir_path
#' character (default: "code-rmd").
#' Path to subdirectory, under a main workflowr directory, where original .Rmd files are saved.
#' It can be only of length = 1.
#' Examples:
#' dir_path = "code-rmd"
#' dir_path = c("code-rmd/subdir1\\subdir2")
#' @param subdirs
#' logical
#' It can be only of length = 1.
#' If TRUE, file listing will also recurse into directories in parameter dir.
#' If FALSE, file listing will be only directly from directories in parameter dir.
#' @param patterns
#' character (default: NULL).
#' Vector of paths to original .Rmd files.
#' If NULL, process all .Rmd files based values in parameters dir and subdirs.
#' If not NULL, process files matching written regular expression.
#' Examples:
#' patterns = "^.*page.*.\[  R , r \]md$")
#' patterns = c("page1.Rmd", ".*page2.*.Rmd")
#' @param commit
#' character (default: FALSE).
#' commit = TRUE creates a separate commit of temporary .Rmd files (temporary saved in "analysis").
#' Suggestion: Use commit = TRUE only after your original .Rmd files saved
#' in subdirectories are tested properly and so are completely ready,
#' otherwise you could have pointlessly many commits.
#' @keywords workflowr, subdirectory
#' @return Final .html file from its original .Rmd file saved in a subdirectory.
#' @export render_html
#' @examples
#' \dontrun{
#'   render_html()
#'   render_html(dir_path = c("code-rmd\\subdir"), subdirs = F)
#'   render_html("code-rmd/subdir", T, c("file1.Rmd", "-.*.[ R , r ]md"))
#' }

render_html <- function(dir_path = "code-rmd", subdirs = T, patterns = NULL, commit = F) {
  # initial settings
  base::setwd(here::here())  # a project working directory could be changed after opening .Rproj
  dir_path <- base::gsub("\\\\", "/", dir_path)  # clearer to work (with one type of slash) with "/"
  initial_checks(dir_path, subdirs, patterns)
  orig_rmd_path <- create_rmd_paths(dir_path, subdirs, patterns)

  # create paths to temporary .Rmd files (with "--") in directory "analysis"
  slash_pos <- base::regexpr("/", orig_rmd_path)  # to cut off the 1st directory in "dir_path"
  temp_rmd_paths <- base::file.path(
    "analysis",
    base::gsub("/", "--",  # a file name cannot contain "/"
               base::substr(orig_rmd_path, slash_pos + 1, base::nchar(orig_rmd_path)))
  )

  # generate temporary .Rmd files in directory "analysis"
  base::mapply(generate_rmd, orig_rmd_path = orig_rmd_path, temp_rmd_path = temp_rmd_paths)

  # commit temporary .Rmd files in directory "analysis" using package "workflowr"
  if (commit == T) {
    workflowr::wflow_git_commit("analysis/*--*Rmd", "separate commit of temporary .Rmd files", all = T)
  }

  # render temporary .Rmd files in directory "analysis" using package "workflowr"
  workflowr::wflow_build(temp_rmd_paths)

  # delete temporary helping .Rmd files
  base::file.remove(temp_rmd_paths)
}

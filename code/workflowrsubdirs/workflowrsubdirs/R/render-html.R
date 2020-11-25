#' @title
#' Render .Rmd files into .html files
#' @description
#' Render only those .Rmd files (based on input parameters) into .html files that meet required criteria.
#' @param dir_path
#' character of length = 1 (default: "code-rmd").
#' Path to a subdirectory, under a main workflowr directory, where .Rmd files for rendering are saved.
#' A directory name can be first instead of using "./" etc. (e.g. "code-rmd" instead of "./code-rmd").
#' Examples:
#' dir_path = "code-rmd"
#' dir_path = c("code-rmd/subdir1\\subdir2")
#' @param subdirs
#' logical of length = 1 (default: TRUE)
#' If TRUE, file listing will recurse into directories in parameter dir.
#' If FALSE, file listing will be only directly from directories in parameter dir.
#' @param patterns
#' character of length > 0 or NULL (default: NULL)
#' A character vector of paths to .Rmd files for rendering.
#' If NULL, process all .Rmd files based values in parameters dir and subdirs.
#' If not NULL, process files matching a regular expression.
#' Examples:
#' patterns = "^.*page.*.\[  R , r \]md$")
#' patterns = c("page1.Rmd", ".*page2.*.Rmd")
#' @param commit
#' character (default: FALSE)
#' If TRUE, a separate commit of temporary .Rmd files (temporary saved in "analysis") is created.
#' It's suggested to use "commit = TRUE" only after original .Rmd files saved
#' in subdirectories are tested properly and so are completely ready,
#' otherwise there could be uselessly many commits.
#' @keywords workflowr, subdirectory
#' @return Final .html files from their original .Rmd files saved in subdirectories.
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

  initial_result <- initial_checks(dir_path, subdirs, patterns)
  if (base::length(initial_result) == 1) {
    if (initial_result == F) {
      #   set "silent" stop()
      #     - no message as a part of stop() isn't written when following 2 lines are used
      #     - stop() will end the whole process (no Continue or Stop button available)
      opt <- base::options(show.error.messages = F)  # this and following line has to be separated
      on.exit(base::options(opt))                    #   meaning on.exit(options(options(show....))) doesn't work
      stop()
    }
  }

  dir_path <- base::gsub("\\\\", "/", dir_path)  # clearer to work (with one type of slash) with "/"
  orig_rmd_path <- create_rmd_paths(dir_path, subdirs, patterns)

  # create paths to temporary (helping) .Rmd files (with "--") in directory "analysis"
  slash_pos <- base::regexpr("/", orig_rmd_path)  # to cut off the 1st directory in "dir_path"
  temp_rmd_paths <- base::file.path(
    "analysis",
    base::gsub("/", "--",  # a file name cannot contain "/"
               base::substr(orig_rmd_path, slash_pos + 1, base::nchar(orig_rmd_path)))
  )

  # generate temporary (helping) .Rmd file(s) in directory "analysis"
  base::mapply(generate_rmd, orig_rmd_path, temp_rmd_paths)

  # commit temporary .Rmd file(s) in directory "analysis"
  if (commit == T) {
    workflowr::wflow_git_commit(temp_rmd_paths, "separate commit of temporary .Rmd files", all = T)
  }

  # render temporary .Rmd files in directory "analysis" into .html files
  workflowr::wflow_build(temp_rmd_paths)

  # delete temporary .Rmd files
  base::file.remove(temp_rmd_paths)
}

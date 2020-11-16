#' @title
#' Create paths to .Rmd files for future rendering
#' @description
#' Create paths to (usually) original .Rmd files, based on input parameters,
#' for future rendering into .html.
#' If no file for rendering is found, processing ends.
#' This function is meant to be called only from function \code{\link{render_html}} so
#' it will be not exported therefore input variables have no default values.
#' @param dir_path
#' character
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
#' character
#' Vector of paths to original .Rmd files.
#' If NULL, process all .Rmd files based values in parameters dir and subdirs.
#' If not NULL, process files matching written regular expression.
#' Examples:
#' patterns = "^.*page.*.\[  R , r \]md$")
#' patterns = c("page1.Rmd", ".*page2.*.Rmd")
#' @keywords workflowr, subdirectory
#' @return Character vector "orig_rmd_path" but stop processing if no file meets criteria.
#' @examples
#' \dontrun{
#'   create_rmd_paths(dir_path = "code-rmd", subdirs = T, patterns = ".*page.*.(R|r)md$")
#' }

create_rmd_paths <- function(dir_path, subdirs, patterns) {
  # initial settings based on "patterns" for mapply() below
  if (base::is.null(patterns)) patterns = "(?i)^.*\\.rmd$"

  # try to create a character vector of .Rmd visible files for further rendering
  orig_rmd_path <- try({    # orig_rmd_path = original .Rmd file paths created based on all input parameters
    base::mapply(           # assignment of try (rather than mapply) is better when something in mapply() fails
      base::list.files,     # if some file doesn't exist then list.files() produces a list instead of a character vector
      path = dir_path,      # lf = list_files
      full.names = T,       # example of a full name: code-rmd/subdir/testfile.Rmd
      recursive = subdirs,  # recursive == T => listing will recurse into subdirectories
      pattern = patterns
      # Notes
      #   all.files = F    # process only visible files
      #   include.dirs = T # include a subdirectory that matches a regular expression in "patterns"
    )
  })
  orig_rmd_path <- base::unname(base::unlist(orig_rmd_path))  # remove unwanted list elements if some of patterns doesn't exist

  # check file paths created from all input parameters
  if (length(orig_rmd_path) == 0) {  # it's not worth to make more checks for separated stops
    stop("No file meets criteria. Processing ends.\n",
         "Possible issues:\n",
         "You wrote path to a file like subdir/filename.Rmd.\n",
         "Case sensitivity isn't met like you can have file file.Rmd but you wrote e.g. File.Rmd or file.rmd and similarly for a regular expression(s).",
         "Chosen file name(s) simply don't exist or chosen regular expression(s) didn't find any matching file.",
         call. = F  # error call (e.g. a function where the error is generated) is not written
    )
  }

  # if more parts of "patterns" point to the same file path, return (process) such file only once
  return(base::unique(orig_rmd_path))
}

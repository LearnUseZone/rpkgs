#' @title
#' Create paths to .Rmd files for future rendering
#' @description
#' Create paths to original .Rmd files intended for future rendering into .html files.
#' If input parameters point to
#'   - a .Rmd file path that doesn't exist then such file is ignored (not rendered).
#'   - a .Rmd file path that exist then such file is rendered in \code{\link{generate_htmls}} using wflow_build().
#' If no file for rendering is found, processing ends.
#' This function is called only from function \code{\link{generate_htmls}} therefore
#' it's not exported and its input variables have no default values.
#' @param dir_path
#' character of length = 1
#' Path to a subdirectory, under a main workflowr directory, where .Rmd files for rendering are saved.
#' A directory name can be first instead of using "./" etc. (e.g. "code-rmd" instead of "./code-rmd").
#' Examples:
#' dir_path = "code-rmd"
#' dir_path = c("code-rmd/subdir1\\subdir2")
#' @param subdirs
#' logical of length = 1
#' If TRUE, file listing will recurse into directories in parameter dir.
#' If FALSE, file listing will be only directly from directories in parameter dir.
#' @param patterns
#' character of length > 0 or NULL
#' A character vector of paths to .Rmd files for rendering.
#' If NULL, process all .Rmd files based values in parameters dir and subdirs.
#' If not NULL, process files matching a regular expression.
#' Examples:
#' patterns = "^.*page.*.\[  R , r \]md$")
#' patterns = c("page1.Rmd", ".*page2.*.Rmd")
#' @keywords workflowr, subdirectory
#' @return A character vector if at least one file meets criteria, stop processing if no file meets criteria.
#' @examples
#' \dontrun{
#'   create_rmd_paths(dir_path = "code-rmd", subdirs = T, patterns = ".*page.*.(R|r)md$")
#' }

create_rmd_paths <- function(dir_path, subdirs, patterns) {
  # initial settings based on "patterns" for mapply() below
  if (base::is.null(patterns)) patterns = "(?i)^.*\\.rmd$"

  # try to create a character vector of .Rmd visible files for further rendering
  orig_rmd_paths <- try({   # orig_rmd_paths = original .Rmd file paths created based on all input parameters
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


  # solving: patterns point to files that don't exist
  #   remove empty (unwanted) list elements when some of patterns point to files that don't exist
  #     empty list element: orig_rmd_paths[[index]] (from mapply() above) returns character(0)
  #     example: render_html(dir_path = "code-rmd\\subdir1", subdirs = T, patterns = c("test-file.Rmd", "test-file-1.Rmd", "test-file-3.Rmd", "^test.*-+.*.Rmd$", ".*Copy.*.Rmd"))
  orig_rmd_paths <- base::unname(base::unlist(orig_rmd_paths))     # create character of length X for situation above but keep matrix for situation below but after this code line run matrix has named all columns

  # solving: mapply() from above creates matrix with more columns
  #   note: when more than one pattern points to the same file path, mapply() from above creates (then it's saved to orig_rmd_paths) a) list or b) matrix with more than 1 columns
  #   create a character vector of all file paths defined by input parameters if orig_rmd_paths created by mapply() from above is matrix with more than 1 column
  #       examples: render_html(dir_path = "code-rmd", subdirs = T, patterns = c("test-file-1.Rmd", "test-file-1.Rmd"))
  #                 render_html(dir_path = c("code-rmd/subdir1"), subdirs = F, patterns = "^.*test.*.[  R , r ]md$")
  if (base::class(orig_rmd_paths)[1] == "matrix") {
    if (base::length(class(orig_rmd_paths)) == 2 &&  # class(orig_rmd_paths) should return "matrix" "array" if it's a matrix
        base::dim(orig_rmd_paths)[2] > 1 &&           # process only matrix with more than 1 column
        base::class(orig_rmd_paths)[2] == "array") {
      orig_rmd_paths <- base::paste0(orig_rmd_paths, collapse = "\t")  # create character of length 1
      orig_rmd_paths <- base::strsplit(orig_rmd_paths, "\t")           # create list of length 1
      orig_rmd_paths <- base::unname(base::unlist(orig_rmd_paths))     # create character of length X
    }
  }


  # remove duplicated rows when more than one pattern points to the same file path
  orig_rmd_paths <- base::unique(orig_rmd_paths)  # unique processes values by columns


  # check file paths created from all input parameters
  if (length(orig_rmd_paths) == 0) {  # it's not worth to make more checks for separated stops
    stop("No file meets criteria.", "\n\n",
         "Possible issues:", "\n",
         "A file path with directories is written - e.g. subdir/filename.Rmd.", "\n",
         "Case sensitivity isn't met - e.g. File.Rmd is written instead of file.Rmd or file.rmd and similarly for a regular expression(s).", "\n",
         "Chosen file names don't exist or chosen regular expression(s) doesn't match any file.",
         call. = F)
  }

  # return file paths (matrix or character vector) for later rendering to .html
  return(orig_rmd_paths)
}

#' @title
#' Create paths to .Rmd files for future rendering
#' @description
#' Create paths to (usually) original .Rmd files, based on input parameters,
#' for future rendering into .html.
#' If path to a file doesn't exist then such file is not rendered and only those files which exist
#' are rendered. If no file for rendering is found, processing ends.
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


  # solving: patterns point to files that doesn't exist
  #   remove empty (unwanted) list elements when some of patterns point to files that doesn't exist
  #     empty list element: orig_rmd_path[[index]] (from mapply() above) returns character(0)
  #     example: render_html(dir_path = "code-rmd\\eToro1", subdirs = T, patterns = c("testToDelete.Rmd", "testToDelete1.Rmd", "testToDelete3.Rmd", "^test.*-+.*.Rmd$", ".*Copy.*.Rmd"))
  orig_rmd_path <- base::unname(base::unlist(orig_rmd_path))     # create character of length X for situation above but keep matrix for situation below but after this code line run matrix has named all columns

  # solving: mapply() from above creates matrix with more columns
  #   note: when more than one pattern points to the same file path, mapply() from above creates (then it's saved to orig_rmd_path) a) list or b) matrix with more than 1 columns
  #   create a character vector of all file paths defined by input parameters if orig_rmd_path created by mapply() in create_rmd_paths() is matrix with more than 1 column
  #       examples: render_html(dir_path = "code-rmd", subdirs = T, patterns = c("testToDelete1.Rmd", "testToDelete1.Rmd"))
  #                 render_html(dir_path = c("code-rmd/eToro1"), subdirs = F, patterns = "^.*test.*.[  R , r ]md$")
  if (base::class(orig_rmd_path)[1] == "matrix") {
    if (base::length(class(orig_rmd_path)) == 2 &&  # class(orig_rmd_path) should return "matrix" "array" if it's a matrix
        base::dim(orig_rmd_path)[2] > 1 &&          # process only matrix with more than 1 column
        base::class(orig_rmd_path)[2] == "array") {
      orig_rmd_path <- base::paste0(orig_rmd_path, collapse = "\t")  # create character of length 1
      orig_rmd_path <- base::strsplit(orig_rmd_path, "\t")           # create list of length 1
      orig_rmd_path <- base::unname(base::unlist(orig_rmd_path))     # create character of length X
    }
  }


  # remove duplicated rows when more than one pattern points to the same file path
  orig_rmd_path <- base::unique(orig_rmd_path)  # unique processes values by columns


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

  # return file paths for later rendering to .html
  return(orig_rmd_path)
}

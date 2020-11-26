#' @title
#' Render .Rmd files to .html files
#' @description
#' Render only those .Rmd files (based on input parameters) that meet required criteria.
#' @param orig_rmd_paths
#' character
#' Paths to original (associated) .Rmd files.
#' @param commit
#' character
#' If TRUE, a separate commit of temporary .Rmd files (temporary saved in "analysis") is created.
#' It's suggested to use "commit = TRUE" only after original .Rmd files saved
#' in subdirectories are tested properly and so are completely ready,
#' otherwise there could be uselessly many commits.
#' @keywords workflowr, subdirectory
#' @return Final .html files from their original .Rmd files saved in subdirectories.
#' @examples
#' \dontrun{
#'   render_to_htmls(orig_rmd_paths, T)
#' }

render_to_htmls <- function(orig_rmd_paths, commit) {
  # create paths to temporary (helping) .Rmd files (with "--") in directory "analysis"
  slash_pos <- base::regexpr("/", orig_rmd_paths)  # to cut off the 1st directory in "dir_path"
  temp_rmd_paths <- base::file.path(
    "analysis",
    base::gsub("/", "--",  # a file name cannot contain "/"
               base::substr(orig_rmd_paths, slash_pos + 1, base::nchar(orig_rmd_paths)))
  )

  # generate temporary (helping) .Rmd file(s) in directory "analysis"
  base::mapply(generate_temp_rmd, orig_rmd_paths, temp_rmd_paths)

  # commit temporary .Rmd file(s) in directory "analysis"
  if (commit == T) {
    workflowr::wflow_git_commit(temp_rmd_paths, "separate commit of temporary .Rmd files", all = T)
  }

  # render temporary .Rmd files in directory "analysis" into .html files
  workflowr::wflow_build(temp_rmd_paths)

  # delete temporary .Rmd files
  base::file.remove(temp_rmd_paths)
}

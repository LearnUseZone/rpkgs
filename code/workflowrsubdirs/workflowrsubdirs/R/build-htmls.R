#' @title
#' Build HTML files
#' @description
#' Multiple R Markdown files saved in directory "code-rmd" and its subdirectories
#' can be specified for rendering to HTML files.
#' The HTML files are always built in the order their associated R Markdown files are listed.
#' @param dir_path
#' character (length = 1; default: "code-rmd").
#' A path directly to directory "code-rmd" or one of its subdirectories with
#' R Markdown files specified in input parameter \code{patterns}.
#' Only a path within a current working workflowr directory is allowed.
#' @param subdirs
#' logical (length = 1; default: TRUE).
#' If \code{FALSE}, file listing will only be directly from a directory in parameter \code{dir_path}.
#' If \code{TRUE}, file listing will also recurse into subdirectories in parameter \code{dir_path}.
#' @param patterns
#' character (length > 0; default: NULL).
#' If \code{not NULL}, one or more regular expressions that only allow files
#' with the extension .rmd or .Rmd ("." is also required) is expected.
#' If \code{NULL}, regular expression "^.*\\.(r|R)md$" is used.
#' @param commit
#' character (default: FALSE).
#' If \code{FALSE}, nothing from actions for \code{TRUE} happens.
#' If \code{TRUE}, a separate commit of temporary (automatically saved and later deleted)
#' R Markdown files saved in directory "analysis" is made and built HTML files contain
#' checked line "R Markdown file" in WORKFLOWR button in tab Checks.
#' Consider to use \code{commit = TRUE} only after original R Markdown files
#' are completely finished, otherwise uselessly many commits are made.
#' @return HTML files from their associated original R Markdown files.
#' @export
#' @examples
#' \dontrun{
#'
#' # Build HTML files from all .(r|R)md files
#'   in "code-rmd" and its subdirectories
#' build_htmls()
#' # Build a single file
#' build_htmls("code-rmd/subdir/sub1", F, "file.Rmd")
#' # Build multiple files (not equivalent examples)
#' build_htmls("code-rmd/subdir", ".*.(r|R)md$")
#' build_htmls("code-rmd/subdir", F)
#' build_htmls("code-rmd\\subdir", T, c("-.*.[ R , r ]md",
#'                                      "file.{1,2}.rmd$",
#'                                      "eFile.Rmd"
#'                                     ), T)
#' }

build_htmls <- function(dir_path = "code-rmd", subdirs = T, patterns = NULL, commit = F) {
  base::setwd(here::here())  # project working directory could be changed after opening .Rproj
  dir_path <- initial_checks(dir_path, subdirs, patterns)
  orig_rmd_paths <- create_rmd_paths(dir_path, subdirs, patterns)  # original .(r|R)md paths
  render_to_htmls(orig_rmd_paths, commit)
}


# ----
# without exporting
# ----


#' @title
#' Make initial checks and preparations
#' @description
#' Stop processing if rendering of R Markdown files to HTML files isn't possible.
#' Make small preparations for further processing if needed, like edit \code{dir_path}.
#' @inheritParams build_htmls
#' @return
#' Nothing if stop rendering is chosen.
#' Nothing if some check doesn't pass but a stop reason is written and then process stops.
#' Original or edited 'dir_path' if all checks pass.

initial_checks <- function(dir_path, subdirs, patterns) {
  # check input parameter "dir_path" (condition 1-2)
  if (base::is.null(dir_path) || dir_path == "")
    stop("Parameter 'dir_path' cannot be NULL nor empty string.", call. = F)
  if (base::length(dir_path) != 1)
    stop("Parameter 'dir_path' can contain only 1 path to a directory.", call. = F)

  # edit input parameter "dir_path" (condition 3)
  #   - remove the 1st "/" to generate correct temporary .Rmd paths (for: whole path, typo)
  #   - remove the last "/" to avoid stop for "if (!file.exists(dir_path))"
  if (dir_path != "code-rmd") {
    dir_path <- base::gsub("\\\\", "/", dir_path)  # clearer to work only with 1 slash type
    dir_path <- base::sub("^./", "", dir_path)     # input dir_path can also be "./code-rmd"
    dir_path <- base::sub(here::here(), "", dir_path)
    while (stringr::str_detect(dir_path, "^/"))
      dir_path <- base::substr(dir_path, 2, nchar(dir_path))
    while (stringr::str_detect(dir_path, ".*/$"))
      dir_path <- base::substr(dir_path, 1, nchar(dir_path) - 1)
  }

  # check input parameter "dir_path" (rest of checks)
  #   - file.exists() doesn't catch path containing more than 1 consecutive "/"
  #   - potential issues with "\" is solved by R error message
  if (base::regexpr("//", dir_path) > 0)  # (condition 4)
    stop("Parameter 'dir_path' contains \"//\" instead of \"/\" or \"\\\\\".", call. = F)

  #   stop processing (works only in a combination with previous 4 conditions) if (r|R)md files are
  #     not within directory 'code-rmd' and also not within a current working directory
  if (dir_path != "code-rmd" && !stringr::str_detect(dir_path, "^code-rmd/.*")) {
    stop("Parameter 'dir_path' doesn't point to a directory within a current working directory or .rmd or .Rmd file is not within the main directory 'code-rmd'.", call. = F)
  }

  if (!file.exists(dir_path))
    stop("Parameter 'dir_path' contains a directory that doesn't exist.", call. = F)


  # check input parameter "subdirs"
  if (!((subdirs %in% c(F, T)) && base::length(subdirs) == 1))
    stop("Parameter 'subdirs' can be only FALSE or TRUE.", call. = F)

  # check input parameter "patterns"
  #   workflowr::wflow_build() expects only files with extension Rmd or rmd otherwise error pops up
  if (!is.null(patterns)) {
    for (pattern_num in 1:base::length(patterns)) {
      if (!stringr::str_detect(  # package "stringr" solves some problems, e.g. with escaping "]", that functions like "base::grepl()" has
        patterns[pattern_num],
        "^.*\\.[\\(, \\[]?\\s*(r|R)\\s*[\\,, \\|]?\\s*(r|R)?\\s*[\\), \\]]?md\\$?$"))
        #   it can still happen that no file will exist but this is solved in create_rmd_paths()
        stop("Parameter 'patterns' has to point only to files with extension .rmd or .Rmd (also that '.' is required).", call. = F)
    }
  }

  # delete unwanted temporary .*--.*.(r|R)md files
  #   - they may occur if weren't deleted in render_to_htmls() - see message for possible fails
  #   - e.g. wflow_git_commit() in render_to_htmls() returns an error if they remain in "analysis"
  if (base::length(
    prohibited_rmd_paths <- base::append(
      base::dir(  # although workflowr accepts only .(r|R)md, delete all (?i).rmd
        path = "analysis", pattern = "(?i)^.*\\-\\-.*.rmd",
        full.names = T,    recursive = T),
      base::dir(
        path = "code-rmd", pattern = "(?i)^.*\\-\\-.*.rmd",
        full.names = T,    recursive = F)
    )) > 0) {

    base::message(
      "Following files contain \"--\" (two hyphens).", "\n",  # I prefer "\n" between commas
      "That isn't allowed in directories \"analysis\" and \"code-rmd\".", "\n\n",
      "This problem could happen for example if:", "\n",
      "  - The relevant original .Rmd files have an error in YAML header", "\n",
      "    (e.g. typo or not allowed text).", "\n",
      "  - The files below were created manually.", "\n\n",
      "Relevant files:", "\n",
      base::paste(prohibited_rmd_paths, collapse = "\n"), "\n\n",
      "Please select one of following options:", "\n",
      "'y' or 'Y'    : listed files will be automatically deleted and script will continue", "\n",
      "anything else : script will stop and therefore listed files have to be managed manually", "\n"
    )

    choice <- base::readline(prompt = "Selection: ")

    if (choice %in% c("y", "Y")) {
      base::file.remove(prohibited_rmd_paths)
    } else {
      base::message("\n", "You chose to stop rendering.")
      #   set "silent" stop()
      #     - no message as a part of stop() isn't written when following 2 code lines are used
      #     - following 2 code lines has to be separated from each other
      opt <- base::options(show.error.messages = F)
      on.exit(base::options(opt))
      stop()
    }
  }
  return(dir_path)
}


#' @title
#' Create paths
#' @description
#' Create paths to original R Markdown files intended for future rendering to HTML files.
#' If no .Rmd file for rendering is found, processing ends.
#' If input parameters refer to an R Markdown file path that
#'   - doesn't exist then such file will be not rendered.
#'   - exist then such file will be rendered in \code{render-to-htmls} using wflow_build().
#' @inheritParams build_htmls
#' @return
#' Nothing if no file meets criteria.
#' A character vector with original R Markdown file paths,
#' if at least one R Markdown file meets criteria.

create_rmd_paths <- function(dir_path, subdirs, patterns) {
  # try to return .(r|R)md visible file paths specified by input parameters
  #   possible returns of mapply() below to variable orig_rmd_paths:
  #   - list where column Value
  #     a) is empty (if no .Rmd file exists)
  #     b) isn't filled in all cells and a filled cell can contain more values
  #     c) is filled in all cells and a filled cell can contain more values
  #   - matrix
  #     a) with 1 column with each cell filled with only 1 value
  #     b) with more than 1 column with each cell filled with only 1 value
  #        where values in the same row are same (rarely happened;
  #        e.g.: build_htmls("code-rmd\\subdir", T, c("file1.Rmd", "file1.Rmd")))
  #   - character vector
  if (base::is.null(patterns)) patterns = "(?i)^.*\\.rmd$"  # initial settings for mapply() below
  orig_rmd_paths <- try({
    base::mapply(          # don't assign mapply() (but "try") because of a potential fail
      base::list.files,
      path = dir_path,
      full.names = T,      # example of full.names: code-rmd/subdir/testfile.Rmd
      recursive = subdirs, # recursive == T => listing will recurse into subdirectories
      pattern = patterns
      # notes
      #   all.files = F    # process only visible files
      #   include.dirs = T # include a subdirectory that matches a regular expression in "patterns"
    )
  })

  # keep a matrix or create a character vector (from list) with removed empty list elements
  if (base::class(orig_rmd_paths[1]) == "list")  # [1] => class(matrix) returns 2 items (not 1)
    orig_rmd_paths <- base::unlist(orig_rmd_paths)

  # create a character vector if orig_rmd_paths is still a matrix with more than 1 column
  #   - unique() processes values by columns => a vector or only 1 column is needed
  #   - example: build_htmls("code-rmd", T, c("test-file-1.Rmd", "test-file-1.Rmd"))
  if (base::class(orig_rmd_paths)[1] == "matrix") { # class(matrix) returns "matrix" "array"
    if (base::dim(orig_rmd_paths)[2] > 1)
      orig_rmd_paths <- base::as.vector(orig_rmd_paths)
  }

  # remove duplicated rows (more than 1 pattern refers to the same .(r|R)md file)
  orig_rmd_paths <- as.character(base::unique(orig_rmd_paths))  # also unname Named chr

  # check an existence of original .(r|R)md files
  if (length(orig_rmd_paths) == 0) {
    stop("No file meets chosen patterns.", "\n\n",
         "Possible issues:", "\n",
         "A file path instead of a file name is written.", "\n",
         "A file name case sensitivity isn't met.", "\n",
         "A file name doesn't exist.", "\n",
         "A regular expression doesn't match any file.",
         call. = F)
  }

  return(orig_rmd_paths)  # return a character vector of .(r|R)md paths
}


#' @title
#' Rendering
#' @description
#' Render R Markdown to HTML files.
#' @param orig_rmd_paths
#' character (length > 0).
#' Paths to original R Markdown files.
#' @param commit
#' see \code{build_htmls}
#' @return
#' Final HTML files from their original R Markdown files.

render_to_htmls <- function(orig_rmd_paths, commit) {
  # note: there's always code-rmd/... at this point => the 1st "/" is always at 9th place

  temp_rmd_names <- base::gsub(  # temporary .(r|R)md file names (for: "code-rmd", "analysis")
    "/", "--", base::substr(orig_rmd_paths, 10, base::nchar(orig_rmd_paths)))  # 9 + 1 = 10

  # ACTIONS over: temporary "code-rmd" .(r|R)md files
  temp_c_rmd_paths <- base::file.path("code-rmd", temp_rmd_names) # CREATE paths; c = code-rmd

  #   CREATE copy to "code-rmd"
  #     - it's needed to be able to use figures generated e.g. by graphics::hist()
  base::file.copy(from = orig_rmd_paths, to = temp_c_rmd_paths)

  #   PREPARE files to delete after .html files are prepared
  delete_c_rmd_paths <- c()
  for (rmd_file in temp_c_rmd_paths) {
    # if an original .(r|R)md file is saved in any subdirectory (not directly in) of "code-rmd"
    if (stringr::str_detect(rmd_file, "^.*\\-\\-.*.(r|R)md")) {
      delete_c_rmd_paths <- base::append(delete_c_rmd_paths, rmd_file)
    }
  }


  # ACTIONS over: temporary "analysis" .(r|R)md files
  temp_a_rmd_paths <- base::file.path("analysis",temp_rmd_names)    # CREATE paths; a -> analysis

  base::mapply(build_temp_rmd, temp_c_rmd_paths, temp_a_rmd_paths)  # GENERATE

  if (commit == T)                                                  # COMMIT
    workflowr::wflow_git_commit(
      temp_a_rmd_paths, "separate commit of temporary .Rmd files", all = T)

  workflowr::wflow_build(temp_a_rmd_paths)                          # RENDER


  # DELETE temporary .(r|R)md files in "code-rmd" and "analysis"
  base::file.remove(delete_c_rmd_paths, temp_a_rmd_paths)
}


#' @title
#' Generate a temporary R Markdown file
#' @description
#' Generate a temporary (helping) R Markdown file, that will be used to generate final HTML file,
#' from its original R Markdown file and temporarily save it into directory "analysis".
#' @param temp_c_rmd_path
#' character (length = 1).
#' A path to an R Markdown file temporarily copied from its original R Markdown file
#' directly to directory "code-rmd".
#' This file will be deleted at the end of function \code{render_to_htmls}.
#' @param temp_a_rmd_path
#' character (length = 1).
#' A path to an R Markdown file that will be temporarily saved in directory "analysis".
#' This file will be deleted at the end of function \code{render_to_htmls}.
#' @return
#' An R Markdown file temporarily saved in directory "analysis".

build_temp_rmd <- function(temp_c_rmd_path, temp_a_rmd_path) {
  base::cat(
    "---\n",
    # YAML header copied (except comments) from an original .Rmd file
    yaml::as.yaml(rmarkdown::yaml_front_matter(temp_c_rmd_path)),
    "---\n\n",
    "**Source file:** ", temp_c_rmd_path,
    "\n\n",

    # r chunk code (not YAML header)
    #   "output.dir" returns directory "analysis" in this case (also tested) and "../" goes one directory up
    #   has to contain path to a temporary .Rmd file in "code-rmd" to be able to use figures generated e.g. by function graphics::hist()
    "```{r child = base::file.path(knitr::opts_knit$get(\"output.dir\"), \"../", temp_c_rmd_path, "\")}\n```",
    file = temp_a_rmd_path,  # file = a name of file that will be created
    sep = "",
    append = F
  )
}


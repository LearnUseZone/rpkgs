#' @title
#' Generate .html files from their original .Rmd files
#' @description
#' Process only those .Rmd files (based on input parameters) that meet required criteria.
#' Use parameters "dir_path", "subdirs" and "patterns" to determine which .rmd or .Rmd files
#' will be rendered to .html files.
#' @param dir_path
#' character of length = 1 (default: "code-rmd").
#' Path to a subdirectory, under a main workflowr directory, where .Rmd files for rendering are saved.
#' A directory name can be first instead of using "./" etc. (e.g. "code-rmd" instead of "./code-rmd").
#' Examples:
#' dir_path = "code-rmd"
#' dir_path = c("code-rmd/subdir1\\subdir2")
#' dir_path = "code-rmd/eToro1/"
#' dir_path = "code-rmd/eToro1\\"
#' @param subdirs
#' logical of length = 1 (default: TRUE)
#' If TRUE, file listing will recurse into directories in parameter dir.
#' If FALSE, file listing will be only directly from directories in parameter dir.
#' @param patterns
#' character of length > 0 or NULL (default: NULL)
#' A character vector of paths to .Rmd files for rendering.
#' If NULL, process all .Rmd files based values in parameters dir and subdirs.
#' If not NULL, process files matching a regular expression.
#' If this parameter isn't NULL then it has always to end
#' with ".rmd", ".Rmd", ".rmd$", ".Rmd$" or
#' a relevant regular expression that after evaluation point to
#' extension ".rmd" or ".Rmd" (case sensitive, "." is also required). This is made
#' in accordance to behavior of package "workflowr"
#' which allows only ".rmd" or ".Rmd" extensions.
#' Examples:
#' patterns = "^.*page.*.\[  R , r \]md$")
#' patterns = c("page1.Rmd", ".*page2.*.Rmd")
#' @param commit
#' character (default: FALSE)
#' If TRUE, a separate commit of temporary .Rmd files (temporary saved in "analysis") is created.
#' It's suggested to use "commit = TRUE" only after original .Rmd files saved
#' in subdirectories are tested properly and so are completely ready,
#' otherwise there could be uselessly many commits.
#' @return Final .html files from their original .Rmd files saved in subdirectories.
#' @export
#' @examples
#' \dontrun{
#'   build_htmls()
#'   build_htmls(dir_path = c("code-rmd\\subdir"), subdirs = F)
#'   build_htmls("code-rmd/subdir", T, c("file1.Rmd", "-.*.[ R , r ]md"))
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
#' Stop processing if rendering of .Rmd to .html files is not possible
#' @description
#' Evaluate if rendering of .Rmd into .html files is possible
#' by checking rules for directories and .Rmd files.
#' Input parameter "dir_path" is edited a) to use only "/" instead of "\\" even if
#' a user uses "\\"; b) remove redundant number of "/" or equivalent number of "\\"
#' at the end of "dir_path", e.g. "code-rmd/subdir//" is changed to "code-rmd/subdir"
#' This function is called only from \code{build_htmls} so its input variables have no default values.
#' @inheritParams build_htmls
#' @return
#' Original or edited 'dir_path' if all checks pass.
#' Nothing if "silent" stop() is applied.
#' Nothing if some check doesn't pass but a stop reason is written and then process stops.

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
#' Create paths to original .Rmd files
#' @description
#' Create paths to original .Rmd files intended for future rendering into .html files.
#' If input parameters point to a .Rmd file path that
#'   - doesn't exist then such file will be not rendered.
#'   - exist then such file will be rendered in \code{render-to-htmls} using wflow_build().
#' If no .Rmd file for rendering is found, processing ends.
#' This function is called only from \code{build_htmls} so its input variables have no default values.
#' @inheritParams build_htmls
#' @return
#' A character vector with original .Rmd file paths if at least one .Rmd file meets criteria.
#' Nothing and stop processing if no file meets criteria.

create_rmd_paths <- function(dir_path, subdirs, patterns) {
  # try to create a matrix / list / character vector of specified .(r|R)md visible files
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
  # possible returns of orig_rmd_paths (from mapply() above)
  # - matrix with 1 column (variable) and all filled rows (observations) with only 1 value per 1 row
  # - matrix with more columns and all filled rows with only 1 value per one cell when values in the same row are same
  # - list where column Value is empty
  # - list where column Value is filled in all cells and can contain more values
  # - list where column Value isn't filled in all cells and if it's filled then it can contain more values
  #   - empty list element: orig_rmd_paths[[index]] returns character(0)
  # - character vector with filled 1 column and 1 row => with 1 value
  # note: when more than one pattern points to the same file path, previous mapply() creates (then it's saved to orig_rmd_paths) a) list or b) matrix with more than 1 columns

  # solving situation: some (not all) of "patterns" point to non-existent files
  #   - remove empty (unwanted) list elements
  #   - base::unlist(orig_rmd_paths)
  #       - create a character vector of length X for situation above if orig_rmd_paths is a list
  #       - do nothing if orig_rmd_paths is any matrix or a character vector
  #   - base::unname(...)
  #     - all columns (even if orig_rmd_paths is any matrix) of a result of base::unlist(...) will have names like V1, V2
  #   - example where only the 1st of "patterns" points to an existing file:
  #     build_htmls("code-rmd\\subdir", F, patterns = c("testfile.Rmd", "test-file-1.Rmd"))
  orig_rmd_paths <- base::unname(base::unlist(orig_rmd_paths))  # possible returns: matrix, character vector


  # solving: mapply() from above creates a matrix with more columns
  #   - create a character vector of all file paths defined by input parameters
  #     if orig_rmd_paths is a matrix with more than 1 column
  #   - unique processes values by columns therefore duplicates needs to be in only 1 column (or vector - in this case)
  #   - examples: build_htmls(dir_path = "code-rmd", subdirs = T, patterns = c("test-file-1.Rmd", "test-file-1.Rmd"))
  #               build_htmls(dir_path = "code-rmd\\subdir", subdirs = F, patterns = "^.*test.*.[  R , r ]md$")
  if (base::class(orig_rmd_paths)[1] == "matrix") {
    if (base::length(class(orig_rmd_paths)) == 2 &&   # class(orig_rmd_paths) should return "matrix" "array" if it's a matrix
        base::dim(orig_rmd_paths)[2] > 1 &&           # process only matrix with more than 1 column
        base::class(orig_rmd_paths)[2] == "array") {
      orig_rmd_paths <- base::paste0(orig_rmd_paths, collapse = "\t")  # create character vector of length 1
      orig_rmd_paths <- base::strsplit(orig_rmd_paths, "\t")           # create list of length 1
      orig_rmd_paths <- base::unname(base::unlist(orig_rmd_paths))     # create character vector of length X
    }
  }


  # remove duplicated rows (more than 1 pattern points to the same .(r|R)md file)
  orig_rmd_paths <- base::unique(orig_rmd_paths)


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

  return(orig_rmd_paths)  # return matrix or character vector of .(r|R)md paths
}


#' @title
#' Render .Rmd to .html files
#' @description
#' Render .Rmd to .html files.
#' This function is called only from \code{build_htmls} so its input variables have no default values.
#' @param orig_rmd_paths
#' character
#' Paths to original .Rmd files.
#' @param commit
#' see \code{build_htmls}
#' @return
#' Final .html files from their original .Rmd files saved in subdirectories.

render_to_htmls <- function(orig_rmd_paths, commit) {
  # note: there's always code-rmd/... at this point => the 1st "/" is always at 9th place

  # ACTIONS over: temporary "code-rmd" .(r|R)md files
  #   CREATE paths
  #     - to work with new files that will be copied to "code-rmd"
  #       - it's needed to be able to use figures generated e.g. by graphics::hist()
  temp_c_rmd_names <- base::gsub("/", "--",                         # c = code-rmd
                                 base::substr(orig_rmd_paths, 10, base::nchar(orig_rmd_paths)))  # 9 + 1 = 10
  temp_c_rmd_paths <- base::file.path("code-rmd", temp_c_rmd_names)

  #   CREATE copy to "code-rmd"
  base::file.copy(from = orig_rmd_paths, to = temp_c_rmd_paths)

  #   PREPARE files to delete after .html files are prepared
  delete_c_rmd_paths <- c()
  for (rmd_file in temp_c_rmd_paths) {
    # if an original .(r|R)md file is saved in any subdirectory of (not directly in) of "code-rmd"
    if (stringr::str_detect(rmd_file, "^.*\\-\\-.*.(r|R)md")) {
      delete_c_rmd_paths <- base::append(delete_c_rmd_paths, rmd_file)
    }
  }


  # ACTIONS over: temporary "analysis" .(r|R)md files
  temp_a_rmd_paths <- base::file.path(                              # CREATE paths; a -> analysis
    "analysis",
    base::gsub("/", "--",
               base::substr(temp_c_rmd_paths, 10, base::nchar(temp_c_rmd_paths)))  # 9 + 1 = 10
  )

  base::mapply(build_temp_rmd, temp_c_rmd_paths, temp_a_rmd_paths)  # GENERATE

  if (commit == T)                                                  # COMMIT
    workflowr::wflow_git_commit(
      temp_a_rmd_paths, "separate commit of temporary .Rmd files", all = T)

  workflowr::wflow_build(temp_a_rmd_paths)                          # RENDER


  # DELETE temporary .(r|R)md files in "code-rmd" and "analysis"
  base::file.remove(delete_c_rmd_paths, temp_a_rmd_paths)
}


#' @title
#' Generate a temporary .Rmd file
#' @description
#' Generate a temporary (helping) .Rmd file from its original .Rmd file
#' and temporarily save it into directory "analysis".
#' This temporarily saved .Rmd file will be used to generate final .html file and
#' will be deleted at the end of function \code{render_to_htmls} after final .html file is prepared.
#' This function is called only from \code{render_to_htmls} so its input variables have no default values.
#' @param temp_c_rmd_path
#' character of length = 1
#' A path to a .Rmd file temporarily copied directly to directory "code-rmd".
#' @param temp_a_rmd_path
#' character of length = 1
#' A name ("--" is a part of those names) of a temporary .Rmd file saved in directory "analysis".
#' @return
#' Temporarily saved .Rmd files in directory "analysis".

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


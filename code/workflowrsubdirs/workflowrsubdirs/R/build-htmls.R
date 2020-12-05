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
  # initial settings
  base::setwd(here::here())  # project working directory could be changed after opening .Rproj
  dir_path <- initial_checks(dir_path, subdirs, patterns)
  orig_rmd_paths <- create_rmd_paths(dir_path, subdirs, patterns)
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
#' @param dir_path
#' see \code{build_htmls}
#' @param subdirs
#' see \code{build_htmls}
#' @param patterns
#' see \code{build_htmls}
#' @return
#' Original or edited 'dir_path' if all checks pass.
#' Nothing if "silent" stop() is applied.
#' Nothing if some check doesn't pass but a stop reason is written and then process stops.

initial_checks <- function(dir_path, subdirs, patterns) {
  # edit input parameter "dir_path" if needed
  if (!base::is.null(dir_path))  # distinguish between null and not null in initial_checks()
    dir_path <- base::gsub("\\\\", "/", dir_path)  # clearer to work only with 1 slash type
  while (stringr::str_detect(dir_path, ".*/$"))
    dir_path <- base::substr(dir_path, 1, nchar(dir_path) - 1)

  # check input parameter "dir_path"
  if (base::is.null(dir_path) || dir_path == "")  # order 1
    stop("Parameter 'dir_path' cannot be NULL nor empty string.", call. = F)
  if (base::length(dir_path) != 1)                # order 2
    stop("Parameter 'dir_path' can contain only 1 path to a directory.", call. = F)
  if (!file.exists(dir_path))
    stop("Parameter 'dir_path' contains a directory that doesn't exist.", call. = F)
  if (base::regexpr("//", dir_path) > 0)  # file.exists() doesn't catch path like dir//subdir, dir///subdir, dir////subdir, etc. (only one "/" has to be used); potential issues with "\" is solved by R error message
    stop("Parameter 'dir_path' contains \"//\" instead of \"/\" or \"\\\\\".", call. = F)
  if (dir_path %in% c("analysis", "code", "data", "docs", "output", "public"))
    stop("Parameter 'dir_path' cannot be a default workflowr directory.", call. = F)

  # check input parameter "subdirs"
  if (!((subdirs %in% c(F, T)) && base::length(subdirs) == 1))
    stop("Parameter 'subdirs' can be only FALSE or TRUE.", call. = F)

  # check input parameter "patterns"
  if (!is.null(patterns)) {
    ## if chosen files are set (not NULL) then check if all chosen files ends with .Rmd or .rmd
    ##   workflowr::wflow_build() expects only files with extension Rmd or rmd otherwise:
    ##     Error: ...  Expected input: Only files with extension Rmd or rmd,  Observed input: ...
    for (pattern_num in 1:base::length(patterns)) {
      if (!stringr::str_detect(  # package "stringr" solves some problems, e.g. with escaping "]", that functions like "base::grepl()" has
        patterns[pattern_num],
        "(?i)^.*\\.[\\(, \\[]?\\s*r\\s*[\\,, \\|]?\\s*r?\\s*[\\), \\]]?md\\$?$"))
        ##   it can still happen that no file will exist but this is solved in create_rmd_paths()
        stop("Parameter 'patterns' has to point only to files with extension .Rmd or .rmd (also that '.' is required).", call. = F)
    }
  }

  # check an existence of unwanted temporary .Rmd files in directory "analysis"
  #   delete such files if chosen by an user
  #   they may occur if these files weren't removed at the end of generate_html() because of some fail
  #   if they are not deleted then calling function wflow_git_commit() in generate_html() ends with an error
  if (base::length(  # if some ".*--.*.Rmd" file exists in "analysis"
    temp_rmd_paths <- base::dir(
      path = "analysis", pattern = "(?i)^.*\\-\\-.*.rmd",
      full.names = T,    recursive = T
    )) > 0) {  # if this part doesn't run then initial_checks() returns NULL

    base::message(
      "Following file names contain \"--\" (two hyphens).", "\n",  # I simply prefer "\n" between commas (if possible)
      "That isn't allowed in directory \"analysis\".", "\n\n",
      "Relevant files:", "\n",
      base::paste(temp_rmd_paths, collapse = "\n"), "\n\n",
      "Please select one of following options:", "\n",
      "'y' or 'Y'    : listed files will be automatically deleted and script will continue", "\n",
      "anything else : script will stop and therefore listed files have to be managed manually", "\n"
    )

    choice <- base::readline(prompt = "Selection: ")

    if (choice %in% c("y", "Y")) {
      base::file.remove(temp_rmd_paths)  # if this part run then initial_checks() returns a number of TRUE equals to number of deleted files
    } else {  # this is not an error it's simply message about a user's choice
      base::message("\n", "You chose to stop rendering.")
      #   set "silent" stop()
      #     - no message as a part of stop() isn't written when following 2 code lines are used
      #     - stop() will end the whole process (no Continue or Stop button available if the package is installed from built source package (.tar.gz file))
      opt <- base::options(show.error.messages = F)  # this and following line has to be separated
      on.exit(base::options(opt))                    #   meaning on.exit(options(options(show....))) doesn't work
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
#' @param dir_path
#' see \code{build_htmls}
#' @param subdirs
#' see \code{build_htmls}
#' @param patterns
#' see \code{build_htmls}
#' @return
#' A character vector with original .Rmd file paths if at least one .Rmd file meets criteria.
#' Nothing and stop processing if no file meets criteria.

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
    stop("No file meets chosen patterns.", "\n\n",
         "Possible issues:", "\n",
         "A file path instead of a file name is written.", "\n",
         "A file name case sensitivity isn't met.", "\n",
         "A file name doesn't exist.", "\n",
         "A regular expression doesn't match any file.",
         call. = F)
  }

  # return file paths (matrix or character vector) for later rendering to .html
  return(orig_rmd_paths)
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
  # create paths to temporary (helping) .Rmd files (with "--") in directory "analysis"
  slash_pos <- base::regexpr("/", orig_rmd_paths)  # to cut off the 1st directory in "dir_path"
  temp_rmd_paths <- base::file.path(
    "analysis",
    base::gsub("/", "--",  # a file name cannot contain "/"
               base::substr(orig_rmd_paths, slash_pos + 1, base::nchar(orig_rmd_paths)))
  )

  # generate temporary (helping) .Rmd file(s) in directory "analysis"
  base::mapply(build_temp_rmd, orig_rmd_paths, temp_rmd_paths)

  # commit temporary .Rmd file(s) in directory "analysis"
  if (commit == T) {
    workflowr::wflow_git_commit(temp_rmd_paths, "separate commit of temporary .Rmd files", all = T)
  }

  # render temporary .Rmd files in directory "analysis" into .html files
  workflowr::wflow_build(temp_rmd_paths)

  # delete temporary .Rmd files
  base::file.remove(temp_rmd_paths)
}


#' @title
#' Generate a temporary .Rmd file
#' @description
#' Generate a temporary (helping) .Rmd file from its original .Rmd file
#' and temporarily save it into directory "analysis".
#' This temporarily saved .Rmd file will be used to generate final .html file and
#' will be deleted at the end of function \code{render_to_htmls} after final .html file is prepared.
#' This function is called only from \code{render_to_htmls} so its input variables have no default values.
#' @param orig_rmd_path
#' character of length = 1
#' A path to an original .Rmd file.
#' @param temp_rmd_path
#' character of length = 1
#' A name ("--" is a part of those names) of a temporary .Rmd file.
#' @return
#' Temporarily saved .Rmd files in directory "analysis".

build_temp_rmd <- function(orig_rmd_path, temp_rmd_path) {
  base::cat(
    "---\n",
    # YAML header copied (except comments) from an original .Rmd file
    yaml::as.yaml(rmarkdown::yaml_front_matter(orig_rmd_path)),
    "---\n\n",
    "**Source file:** ", orig_rmd_path,
    "\n\n",

    # r chunk code (not YAML header)
    #   "output.dir" returns directory "analysis" in this case (also tested) and "../" goes one directory up
    "```{r child = base::file.path(knitr::opts_knit$get(\"output.dir\"), \"../", orig_rmd_path, "\")}\n```",

    file = temp_rmd_path,  # file = a name of file that will be created
    sep = "",
    append = F
  )
}


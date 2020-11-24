[workflowrsubdirs](https://github.com/LearnUseZone/workflowrSubfolders)
================
LearnUseZone
Last update: 2020-11-24 09:38 GMT+2

  - [Purpose](#purpose)
  - [General rules](#general-rules)
      - [Avoid problems with YAML header of .Rmd files in
        subdirectories](#avoid-problems-with-yaml-header-of-.rmd-files-in-subdirectories)
  - [Briefly about package function
    render\_html()](#briefly-about-package-function-render_html)
      - [Main steps](#main-steps)
      - [Other important information](#other-important-information)
  - [Installation](#installation)
      - [Try following steps if the package weren’t installed
        successfully:  
        ](#try-following-steps-if-the-package-werent-installed-successfully)
  - [Needed packages](#needed-packages)
  - [Example](#example)
      - [Usage of workflowrsubdirs after it's
        installed](#usage-of-workflowrsubdirs-after-its-installed)
  - [Additional notes](#additional-notes)

## Purpose

  - It’s an optional extension of package
    [workflowr](https://github.com/jdblischak/workflowr) in order to be
    able to render .html pages from .Rmd files saved in subdirectories
    of a workflowr project.
      - Use workflowr if .Rmd files from directory "analysis" are
        rendered.
      - Use workflowrsubdirs if .Rmd files from subdirectories are
        rendered.
  - I suggest to create a new directory, for your .Rmd files saved in
    subdirectories, directly in your workflowr project’s working
    directory (and not in directory "analysis").
      - For example: create directory "code-rmd" in the same directory
        as directory "code".

## General rules

  - This package is an extension for package "workflowr" therefore it’s
    needed to have the same structure of directories "analysis", "code"
    and "docs" as for workflowr.
      - When you create your directory for .Rmd files in subdirectories
          - it has to be in the same directory as previous 3
            directories.
          - I suggest to create a directory "code-rmd" as this is also
            the default directory in function generate\_html() (see
            below).
  - .Rmd files containing "--" (two hyphens) are not allowed in
    directory "analysis".

### Avoid problems with YAML header of .Rmd files in subdirectories

  - Example of inline R code that can cause problems with rendering .Rmd
    file to .html file:  
    date:    "\`r paste("Last update:",
    format(lubridate::with\_tz(as.POSIXct(Sys.time()) + 7200, tzone =
    "GMT"), "%Y-%m-%d %H:%M GMT+2"))\`"  
  - Summary:
      - Use single quotation marks for inline R codes in YAML header in
        .Rmd files saved in directory analysis, e.g.:  
        "\`r paste('Last update:', …
      - Use escaped double quotation marks for inline R codes in YAML
        header in .Rmd files saved in subdirectories, e.g.:  
        "\`r paste(\\"Last update:\\", …
  - More information:
      - If a single quotation mark (') is used then two consecutive
        single quotation marks ('') are created in YAML header in a
        temporary .Rmd file (will be deleted after .html file is
        rendere) that is temporarily created in directory analysis (this
        file is used to render a final .html file).
          - Usage of a single quotation marks is working for
            workflowr::wflow\_build("analysis/test-file-date.Rmd").
      - If a double quotation mark (") is used then error is displayed.
          - The same error is displayed also for
            workflowr::wflow\_build("analysis/test-file-date.Rmd").
      - If an escaped single quotation mark (\\’) is used then error is
        displayed.
          - The same error is displayed also for
            workflowr::wflow\_build("analysis/test-file-date.Rmd").
      - If an escaped double quotation mark (\\") is used then rendering
        a temporary .Rmd file to .html file works.
          - An error is displayed for
            workflowr::wflow\_build("analysis/test-file-date.Rmd").

## Briefly about package function render\_html()

  - This package contains more functions but only render\_html() is
    visible for a user.
  - Package “workflowr” is used for commits during processing and also
    for building .html pages.

#### Main steps

1.  Evaluate if rendering of .html files is possible. If some check
    doesn’t pass, write a reason and stop processing.
      - Used package function: initial\_checks()
2.  Create paths (in a form of matrix with characters or character
    vector) to original .Rmd files intended for future rendering into
    .html files.
      - Used package function: create\_rmd\_paths()
3.  Generate a temporary (helping) .Rmd file, from its original .Rmd
    file, in directory “analysis”.
      - Used package function: generate\_rmd()
4.  If commit == TRUE, create a commit of these temporary (helping) .Rmd
    files with text “separate commit of temporary .Rmd files”.
      - Used function: workflowr::wflow\_git\_commit()
      - If these temporary (helping) .Rmd files are not committed before
        workflowr::wflow\_build(), following unwanted error message is
        written after opening of any prepared .html file and
        left-clicking on “workflowr” button under tab "Checks": "R
        Markdown file: uncommitted changes"
          - This is also in line with "workflowr" package.
5.  Render temporary .Rmd files in directory “analysis” into .html
    files.
      - Used function: workflowr::wflow\_build()
      - Final .html files are prepared (also package workflowr is set in
        this way) in directory "docs" (for GitHub) or directory "public"
        (for GitLab). Each such file name contains "--" which are
        delimiters for paths to original .Rmd files paths.
6.  Delete temporary (helping) .Rmd files from directory “analysis”.

#### Other important information

  - Even when relevant temporary files were committed separately (see
    above), it’s necessary to commit all files after .html files are
    prepared
      - Commit of all removed (see below) temporary .Rmd files and new
        .html files).
  - If other than workflowr project is originally opened using its
    .Rproj file then this function fails because
    "base::setwd(here::here())" in this function sets a working
    directory to an original .Rproj working directory regardless a
    current working directory (e.g. set after opening .Rproj).
    Potentially if a relevant .Rproj working directory have the same
    structure then it could work but I didn’t test it, yet.
      - This type of issue can be considered after running
        "workflowrsubdirs::generate\_html()", the following error
        message arises in "Console" tab: Error in
        base::mapply(generate\_rmd, dir, file\_path, temp\_file) :
        zero-length inputs cannot be mixed with those of non-zero length
          - "base::mapply(generate\_rmd, dir, file\_path, temp\_file)"
            is called inside function
            "workflowrsubdirs::generate\_html()".
  - Add better explanation for input parameters
      - .Rmd or .rmd at the end of pattern is required (also that “.” is
        required)

## Installation

  - Option 1
      - Clone this Git [rpkgs
        repository](https://github.com/LearnUseZone/rpkgs).
      - Open
        rpkgs/code/workflowrsubdirs/workflowrsubdirs/workflowrsubdirs.Rproj
        in RStudio.
      - Build (from top menu) -\> Install and Restart (Ctrl+Shift+B)
  - Option 2
      - Do steps from Option 1 until opening .Rproj in RStudio in Option
        1 (include also this step)
      - Build (from top menu) -\> Build Source Package -\> wait until
        .tar.gz file is created.
      - Run R code: install.packages(\<path\_to\_tar.gz\_file\>, repos =
        NULL, type = "source")

### Try following steps if the package weren’t installed successfully:  

1.  Check if the package is in RStudio "Packages" tab and if yes,
    uninstall it.
2.  Check if directory "workflowrsubdirs" exists within directory
    “library” with your installed R packages and if it exists, delete
    it.
3.  Restart R session e.g. using RStudio -\> Session -\> Restart R.
4.  Install the package again using  
    a) Install and Restart (Ctrl+Shift+B) or  
    b) install.packages(\<path\_to\_tar.gz\_file\>, repos = NULL, type =
    "source").

## Needed packages

  - Before you can fully use this package make sure you have installed
    following packages (their loading isn’t necessary):  
      - base
      - here
      - knitr
      - rmarkdown
      - stringr
      - workflowr
      - yaml

## Example

  - At the beginning a working directory of package "workflowr" needs to
    be prepared. You can find relevant steps together with more
    information like usage of (required and optional) workflowr
    subdirectories
    [here](https://jdblischak.github.io/workflowr/articles/wflow-01-getting-started.html)
  - You can use directory “code” for codes that might not be appropriate
    to include in R Markdown format (e.g. for pre-processing the data,
    or for long-running code). You can have also subdirectories here.
  - If you want to render .Rmd files (showing results of your .R files
    from directory "code") to .html files using purely package
    "workflowr", save relevant .Rmd files into directory "analysis".
  - If you like to have the same structure as .R files also for your
    .Rmd files, you can create a new directory, e.g. "code-rmd", and
    create relevant subdirectories together with associated .Rmd files
    (these .Rmd files could be also in subdirectories of directory
    “code” but I think it could be less organized or clear).
      - Don’t use directory "analysis" for this purpose. Now, because
        you have .Rmd files in subdirectories under directory code-rmd,
        use this package workflowrsubdir to render .html files.
  - Examples: Consider following files  
      - "code-rmd/subdir/testfile1.Rmd",
      - "code-rmd/subdir/testfile2.rmd",
      - "code-rmd/subdir/testdir/testfile1.Rmd",
      - "code-rmd/subdir/testdir/my-analyses.Rmd"
      - If you want to render all those files, you can use several
        options, like:
          - workflowrsubdirs::generate\_html() \# if there are other
            files in "code-rmd" or it’s subdirectories, those files will
            be processed, too
          - workflowrsubdirs::generate\_html(dirs = "code-rmd/subdir")
            \# all files in directories and subdirectories of
            "code-rmd/subdir" will be processed
          - workflowrsubdirs::generate\_html(dirs =
            "code-rmd\\\\subdir") \# all files in directories and
            subdirectories of "code-rmd/subdir" will be processed
          - workflowrsubdirs::generate\_html(dirs = "code-rmd/subdir",
            orig\_rmd\_patterns = ".\*.(r|R)md$")
          - workflowrsubdirs::generate\_html(dirs = "code-rmd/subdir",
            orig\_rmd\_patterns = c("^test.\*.rmd$", "file1.Rmd",
            "-.\*.\[ R , r \]md"))
  - Important note: If orig\_rmd\_patterns isn’t NULL then it always has
    to end with .rmd, .Rmd, .rmd$, .Rmd$ or a relevant regular
    expression that after evaluation point to one of those 4 extensions.
      - This is made in accordance to behaviour of package "workflowr"
        which allows only .rmd or .Rmd extensions.
  - Note: A real example with .Rmd files in subdirectories (under
    directory codeRmd in this case) can be found
    [here](https://github.com/LearnUseZone/workflowrSubfolders).
      - Use this only for your better overview of managing directories
        because this package is enhanced against the original version
        used in "workflowrSubfolders".

### Usage of workflowrsubdirs after it's installed

1.  After workflowrsubdirs is installed, open your workflowr project
    (run .Rproj file).
2.  Use workflowrsubdirs::generate\_html().
3.  Remember that although commit of temporary .Rmd files were made
    within function workflowrsubdirs::generate\_html(), you still have
    to commit the rest of files. You can use for this purpose e.g.:

<!-- end list -->

  - 3a. GitHub Desktop, Sourcetree or other Git desktop client.
  - 3b. Git Bash (also set as Terminal in RStudio) with git commands
    like:  
    git branch -a \# I prefer to check whitch branch is checked out  
    git add “.”  
    git commit -m “tested: package workflowrsubdirs”  
    git push origin master
  - 3c. Functions of package workflowr like:  
    workflowr::wflow\_git\_commit(c(“docs/subPages1–testPrint1.html”,
    “docs/subPages2–testPrint2.html”), “tested: package
    workflowrsubdirs with workflowr”, all = TRUE)  
    workflowr::wflow\_use\_github("LearnUseZone", "workflowrSubfolders")
    \# usually choose a default option which is 2  
    workflowr::wflow\_git\_push() \# use your credentials to push your
    changes to checked out branch.  

## Additional notes

  - I assume that you are using RStudio and therefore some parts of this
    or other documents may be focused on this assumption but of course
    the relevant associated steps (you need to know them) work also if
    you don't use RStudio.
  - I’m still working on improvements therefore it can still be found
    for example
      - some placeholders like "tests-generate\_rmd.R" and
      - notes for future improvements.
  - Initial inspiration for this package is from
    [here](https://github.com/jdblischak/workflowr/issues/95).  
  - A related discussion with John Blischak (a creator of package
    workflowr) about a base set-up can be found
    [here](https://github.com/jdblischak/workflowr/issues/220).

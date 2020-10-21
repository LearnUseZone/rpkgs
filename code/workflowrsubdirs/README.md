[workflowrsubdirs](https://github.com/LearnUseZone/workflowrSubfolders)
================
LearnUseZone
Last update: 2020-10-17 16:25 GMT+2

  - [Purpose](#purpose)
      - [Briefly about package
        functions](#briefly-about-package-functions)
  - [Installation](#installation)
      - [If there are problems with
        installation](#if-there-are-problems-with-installation)
  - [Needed packages](#needed-packages)
  - [Example](#example)
      - [Usage of workflowrsubdirs after it’s installed from the source
        tar.gz
        file](#usage-of-workflowrsubdirs-after-its-installed-from-the-source-tar.gz-file)
  - [Additional notes](#additional-notes)

## Purpose

  - It’s an optional extension of package
    [workflowr](https://github.com/jdblischak/workflowr) in order to be
    able to render .html pages from .Rmd files saved in subdirectories
    of a workflowr project.
      - Use workflowr if .Rmd files from directory “analysis” are
        rendered.
      - Use workflowrsubdirs if .Rmd files from subdirectories are
        rendered.
  - I suggest to create a new directory, for your .Rmd files saved in
    subdirectories, directly in your workflowr project’s working
    directory (and not in directory “analysis”).
      - For example: create folder “codeRmd” in the same directory as
        folder “code”.

### Briefly about package functions

  - As usually, information about implemented function can be found
    using help (F1). Information below simply adds even more clarity.  
  - The base logic of 2 main functions generate\_rmd() and
    generate\_html() (see below) comes from
    [here](https://github.com/jdblischak/workflowr/issues/95#issuecomment-360094662)
    but then they are upgraded for a better consistency with package
    workflowr.  
  - [Here](https://github.com/jdblischak/workflowr/issues/220) can be
    found my discussion of above mentioned upgrades with creator of
    package workflowr.

#### generate\_rmd()

  - It cannot be called.
  - It’s only used in generate\_html().
  - @examples (file generate\_rmd.R) are set to not run.
  - If you want to use devtools::check() also with checking @examples,
    then remember that this package is an extension of workflowr and in
    order to ensure that example not fails, it’s needed
      - to have the same structure of some folders (analysis, code,
        codeRmd, docs) as for workflowr, or
      - to adjust code lines referencing to any path and it’s not
        effective to do it only because of checking.

#### generate\_html()

  - It can be called.
  - It manages process of rendering .html pages from .Rmd files from
    subdirectories, so this is the only one function needed to be
    called.
  - Rendered .html files are again prepared in folder “docs” (for
    GitHub) or folder “public” (for GitLab). Each such file name
    consists of “‐‐” which are delimiters for paths to original .Rmd
    files paths.
  - After look at workflowr button of opened .html file, under tab
    “Checks” can be found “R Markdown file: uncommitted changes”. This
    is in line with workflowr package and it means that also your
    temporary files were committed separately, these new .html files
    have to be commited, too (e.g. using
    workflowr::wflow\_git\_commit())
  - If other than workflowr project is originally opened using its
    .Rproj file then this function fails because
    “base::setwd(here::here())” in this function sets a working
    directory to an original .Rproj working directory regardless a
    current working directory (e.g. set after opening .Rproj).
    Potentially if a relevant .Rproj working directory have the same
    structure then it could work but I didn’t test it, yet.
      - This type of issue can be considered after running
        “workflowrsubdirs::generate\_html()”, the following error
        message arises in “Console” tab: Error in
        base::mapply(generate\_rmd, dir, file\_path, temp\_file) :
        zero-length inputs cannot be mixed with those of non-zero length
          - “base::mapply(generate\_rmd, dir, file\_path, temp\_file)”
            is called inside function
            “workflowrsubdirs::generate\_html()”.

## Installation

  - Download file “workflowrsubdirs\_0.0.0.0200.tar.gz” from
    [GitHub](https://github.com/LearnUseZone/workflowrSubfolders/tree/master/code).
  - Run: install.packages(path\_to\_tar.gz\_file, repos = NULL,
    type=“source”)

### If there are problems with installation

  - Try following steps if the package weren’t installed successfully:  
    1.  Check if the package is in RStudio “Packages” tab and if yes,
        uninstall it.
    2.  Check if folder “workflowrsubdirs” exists within folder
        “library” with your installed R packages and if it exists,
        delete it.
    3.  Restart R session e.g. by RStudio -\> Session -\> Restart R.
    4.  Install the package again using install.packages().

## Needed packages

  - Before you can fully use this package make sure you have installed
    following packages (their loading isn’t necessary):  
      - base
      - here
      - rmarkdown
      - workflowr
      - yaml

## Example

  - If you have created a workflowr project, it’s required to create
    .Rmd files in folder “analysis” in order to render them to .html
    files.
  - You can use directory “code” for codes that might not be appropriate
    to include in R Markdown format (e.g. for pre-processing the data,
    or for long-running code). You can have also subdirectories here.
  - If you want to have .Rmd files showing results of your .R files from
    folder “code”, you have to create them in folder “analysis” if you
    want to use purely package workflowr.
  - If you like to have the same structure as .R files also for your
    .Rmd files, you can create a new folder, e.g. codeRmd, and create
    relevant subdirectories together with associated .Rmd files (these
    .Rmd files could be also in subdirectories of folder “code” but I
    think it could be less organized or clear).
  - Now, because you have .Rmd files in subdirectories under directory
    codeRmd, use this package workflowrsubdir to render .html files.
  - As a real example, please feel free to look at structure of this
    [GitHub
    repository](https://github.com/LearnUseZone/workflowrSubfolders).
  - More about usage of workflowr folders is
    [here](https://jdblischak.github.io/workflowr/articles/wflow-01-getting-started.html).

### Usage of workflowrsubdirs after it’s installed from the source tar.gz file

1.  After workflowrsubdirs is installed, open your workflowr project
    (run .Rproj file).
2.  Use workflowrsubdirs::generate\_html().

<!-- end list -->

  - If you downloaded my
    [repository](https://github.com/LearnUseZone/workflowrSubfolders),
    then you can test it by running an example code that you can find
    also in help for function “generate\_html()”:
    workflowrsubdirs::generate\_html(“codeRmd”,
    c(“subPages1/testPrint1.Rmd”, “subPages2/testPrint2.Rmd”), T)

<!-- end list -->

3.  Remember that although commit of temporary .Rmd files were made
    within function workflowrsubdirs::generate\_html(), you still have
    to commit the rest of files. You can use for this purpose e.g.:

<!-- end list -->

  - 3a. GitHub Desktop
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
    workflowr::wflow\_use\_github(“LearnUseZone”, “workflowrSubfolders”)
    \# usually choose a default option which is 2  
    workflowr::wflow\_git\_push() \# use your credentials  
      - As it’s shown in 3b – simply make sure that you are in a correct
        branch and feel free to use workflowr functions which push your
        changes to checked out branch.

## Additional notes

  - I assume that you are using RStudio and therefore some parts of this
    or other documents may be focused on this assumption but of course
    the relevant associated steps (you need to know them) work also if
    you don’t use RStudio.
  - I’m still working on improvements therefore it can still be found
    for example
      - some placeholders like “tests-generate\_rmd.R” and
      - notes for future improvements.
% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/initial-checks.R
\name{initial_checks}
\alias{initial_checks}
\title{Check if rendering .html files from .Rmd files from subdirectories is possible}
\usage{
initial_checks(dir = "code-Rmd", path_orig_Rmd = NULL)
}
\arguments{
\item{dir}{character (default: "code-Rmd").
Path to a directory, under a main workflowr subdirectory, where original Rmd files are saved.}

\item{path_orig_Rmd}{character (default: NULL).
Vector of paths to original .Rmd files. These file paths start with a name of the 1st subdirectory of a directory specified in variable "dir".
Example when directories subPagesX are saved in directory dir = "code-Rmd":
file_path = c("subPages2/testPrint1.Rmd", "subPages3/testPrint2.Rmd")
file_path = c("subPages2\\testPrint1.Rmd", "subPages3\\testPrint2.Rmd")}
}
\value{
None, but stop processing if any of checks fails.
}
\description{
Check if rendering .html files from .Rmd files from subdirectories is possible
}
\examples{
\dontrun{
  initial_checks(dir, path_orig_Rmd)
}
}
\keyword{subdirectory}
\keyword{workflowr,}
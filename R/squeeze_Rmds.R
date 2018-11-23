## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' A Simple Wrapper for Tangling Code in Bookdown Projects
#'
#' This function extracts R code from \code{.Rmd} files belonging to bookdown projects.
#' By default, it scans the working directory for \emph{numbered} \code{.Rmd} files and passes them to \code{knitr::purl()} which tangles the code chunks whithin to \code{.R} files.
#' It is intended as a convenience function for students working with \emph{Introduction to Econometrics with R},
#' an interactive online-companion to the textbook \emph{Introduction to Econometrics} (Stock & Watson 2015). The companion can be found \href{https://www.econometrics-with-r.org}{here}. The function works with arbitrary \code{.Rmd} files and is useful for working with other bookdown projects, too.
#' @param dir Path of the directory containing the \code{.Rmd} files to be tangled. Default is the current working directory.
#' @param out Path to the output directory where the tangled codes will be saved. Points the \code{./Rcode} by default.
#' @param ... Further arguments passed to \code{knitr::purl()}.
#' @param numbered \code{logical}. If \code{TRUE}, only numbers in the original file names are retained.
#' @param prefix \code{character}. A prefix to be added to the \code{.R} file names.
#' @keywords purl, bookdown
#' @export
#' @examples
#' \dontrun{
#' # tangle numbered chapters of a bookdown project at working directory
#' squeeze_rmds(numbered = TRUE, prefix = "Chapter_")
#' }

squeeze_rmds <- function(dir = ".",
                         numbered = T,
                         out  = "Rcodes",
                         prefix = "", ...) {

  if(numbered) {
    # generate a vector of Rmd file names;
    Rmds <- list.files(path = dir, pattern = "^[0-9]")
    # generate .R file paths from Rmd names
    rcodes <- sub("\\D*(\\d{2}).*", "\\1", Rmds)
    rcodes <- sub('^0+([1-9])', '\\1', rcodes)
  } else {
    # generate a vector of Rmd filenames;
    Rmds <- list.files(path = dir)
    # omit file extension
    rcodes <- tools::file_path_sans_ext(Rmds)
  }
  rcodes <- paste0(out, prefix, rcodes, ".R")

  # tangle R code from Rmd files
  for(i in seq_along(Rmds)) {
    knitr::purl(input = paste0(dir, Rmds[i]),
                output = rcodes[i],
                documentation = 0, ...)
  }
}

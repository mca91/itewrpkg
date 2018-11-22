#' Download Files From GitHub Repository
#'
#' This function allows to download files from a specific folder located in a repository at GitHub.com. It is intended as a convenience function for students working with \emph{Introduction to Econometrics with R},
#' an interactive online-companion to the textbook \emph{Introduction to Econometrics} (Stock & Watson 2015). The companion can be found \href{https://www.econometrics-with-r.org}{here}. The function works with any GitHub repository.
#' @param repo \code{character} A GitHub repository. Must be specified in the form \code{"username/repository"}.
#' Defaults to \code{"mca91/EconometricsWithR"}.
#' @param folder A \code{character} specifying the folder containing the files to be downloaded. Defaults to the \code{master} folder.
#' @param type A vector of type \code{character} specifying the file type(s) to be downloaded. All file types will be downloaded by default.
#' @param out A \code{character} specifying the path to the output directory where the files will be saved. Defaults to the current working directory.
#' @keywords download github
#' @export
#' @examples
#' # download datasets from "mca91/EconometricsWithR"
#' get_repo_files(folder = "master/data")
#' @section References:
#' Stock, J. H. and Watson, M. W. (2015). \emph{Introduction to Econometrics}, Third Update, Global Edition. Pearson Education Limited.

get_repo_files <- function(repo = "mca91/EconometricsWithR",
                           folder = "master",
                           type = "any",
                           out = ".") {

  # generate URL to raw content of the folder in repo
  repo_raw <- paste0("https://raw.githubusercontent.com/", repo, "/", folder, "/")
  # generate URL to folder in git repo
  repo <- paste0("https://github.com/", repo, "/tree/", folder, "/")
  # scrape names of files located in the folder
  filenames <- get_repo_filenames(repo)

  # use subset of files if type != "any"
  if (type != "any") {
    filenames <- c(unlist(
      sapply(type, function(x) filenames[grep(pattern = paste0(".", x, "$"), x = filenames)], USE.NAMES = F)
      ))
  }

  # download files
  if(!is.null(filenames)) {
    cat("The following files were found:\n",
        paste(filenames, collapse = "\n"), " ", paste("Downloading to", out, "\n"), "This might take a few seconds...", "", sep = "\n")

    n <- length(filenames)
    # create progress bar
    pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
    for(i in seq_along(filenames)) {
      utils::download.file(paste0(repo_raw, filenames[i]),
                           destfile =  paste0(out, filenames[i]),
                           quiet = T)
      utils::setTxtProgressBar(pb, i)
    }
    cat("\n")
    close(pb)
    cat("... finished!")
  } else {
    cat('No files were found.')
  }

}



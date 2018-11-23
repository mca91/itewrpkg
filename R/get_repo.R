#' Download a GitHub Repository
#'
#' This function allows to download a whole repository from GitHub.com. It is
#' intended as a convenience function for students working with
#' \emph{Introduction to Econometrics with R}, an interactive online-companion
#' to the textbook \emph{Introduction to Econometrics} (Stock & Watson 2015), but works with any public GitHub repository.
#' The companion can be found \href{https://www.econometrics-with-r.org}{here}.
#' @param repo A \code{character} string stating the name of the GitHub repository to be
#'   downloaded. Must be specified in the form \code{"username/repository"}.
#' @param out A \code{character} string specifying the path to the output
#'   directory where the repository files will be saved. Defaults to the current
#'   working directory.
#' @keywords download github repository
#' @export
#' @details The function downloads the zipped contents (\code{master.zip}) of
#'   the master branch from the GitHub repository supplied to the argument
#'   \code{repo} to the directory specified in \code{out} and unzips the archive
#'   afterwards in \code{<out>/Repository/}.
#' @examples
#' \dontrun{
#' # download the bookdown projekt repository
#' # "mca91/EconometricsWithR"
#' get_repo(repo = "mca91/EconometricsWithR")
#' }
#' @section References: Stock, J. H. and Watson, M. W. (2015).
#'   \emph{Introduction to Econometrics}, Third Update, Global Edition. Pearson
#'   Education Limited.

get_repo <- function(repo = NULL, out = ".") {

  # generic error message
  msg <- "Something went wrong. Did you provide valid repository name? See the documentation for futher details."

  if(!is.null(repo)) {

   # generate download link and path to .zip file
   dl_link <- paste0("https://github.com/", repo, "/archive/master.zip")
   zip_file <- paste0(out, "/master.zip")

   # check if url is valid
   if(RCurl::url.exists(dl_link)) {
     # download zipped master branch of the repository `repo`
     utils::download.file(url = dl_link,
                          destfile = zip_file)
     # unzip master.zip
     utils::unzip(zipfile = zip_file,
                  exdir = out)

     # silently delete the .zip archive
     invisible(file.remove(zip_file))
     } else {
       cat(msg)
     }
   } else {
    cat(msg)
  }

}


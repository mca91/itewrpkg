#' Get Filenames from GitHub Repository
#'
#' This function allows to retrieve filenames from a GitHub repository using the GitHub Contents API.
#' @param repo A \code{character}. Path to a GitHub repository tree URL.
#' @keywords GitHub filenames
#' @export
#' @examples
#' # get filenames from 'EconometricsWithR/master/data'
#' get_repo_filenames(repo = "https://github.com/mca91/EconometricsWithR/tree/master/data")

get_repo_filenames <- function(repo) {
  # Parse GitHub tree URL: https://github.com/{owner}/{repo}/tree/{ref}/{path}
  m <- regexec("github\\.com/([^/]+/[^/]+)/tree/([^/]+)(/[^?]*)?", repo)
  parts <- regmatches(repo, m)[[1]]
  owner_repo <- parts[2]
  ref        <- parts[3]
  path       <- gsub("^/|/+$", "", parts[4])  # strip leading/trailing slashes

  api_url  <- paste0("https://api.github.com/repos/", owner_repo, "/contents/", path, "?ref=", ref)
  contents <- jsonlite::fromJSON(api_url)

  if (is.data.frame(contents)) {
    contents$name[contents$type == "file"]
  } else {
    character(0)
  }
}

#' Get Filenames from GitHub Repository
#'
#' This function allows to scrape filenames from a GitHub repository.
#' @param repo A \code{character}. Path to a GitHub repository.
#' @keywords GitHub filenames
#' @importFrom magrittr %>%
#' @export
#' @examples
#' # get filnames from 'EconometricsWithR/master/data'
#' get_repo_filenames(repo = "https://github.com/mca91/EconometricsWithR/tree/master/data")

get_repo_filenames <- function(repo) {
  xml2::read_html(repo) %>%
    rvest::html_nodes(css = ".files") %>%
    rvest::html_table() %>%
    .[[1]] %>%
    .[-(1:2), 2]
}

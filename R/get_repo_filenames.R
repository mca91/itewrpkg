#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()

get_repo_filenames <- function(repo) {
  require(rvest)
  read_html(repo) %>%
    html_nodes(css = ".files") %>%
    html_table() %>%
    .[[1]] %>%
    .[-(1:2), 2]
}
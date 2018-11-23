#' Download Book Materials
#'
#' This function allows you to download supplements for \emph{Introduction to Econometrics with R},
#' an interactive online-companion to the textbook \emph{Introduction to Econometrics} (Stock & Watson 2015). The book can be found \href{https://www.econometrics-with-r.org}{here}.
#' @param dir A \code{character} specifying the path to the directory where the files will be saved. Defaults to the current working directory.
#' @details The function is a wrapper for \code{get_repo_files()} and \code{squeeze_rmds()}.
#' It downloads materials such as datasets and R codes from the GitHub repository of the book (\emph{mca91/EconometricsWithR}) and saves them to the current working directory (or a location of choice provided to the argument \code{dir}) according to the following structure:
#' \itemize{
#'   \item \code{/ITEWR/Rmds} (.Rmd files)
#'   \item \code{/ITEWR/Data} (Datasets)
#'   \item \code{/ITEWR/Rcodes} (R Scripts for each chapter)
#' }
#' @keywords download supplements
#' @export
#' @examples
#' \dontrun{
#' # download materials
#' get_materials_itewr()
#' }
#' @section References:
#' Stock, J. H. and Watson, M. W. (2015). \emph{Introduction to Econometrics}, Third Update, Global Edition. Pearson Education Limited.

get_materials_itewr <- function(dir = ".") {

  # create folders
  dir.create(paste0(dir, "/ITEWR/Rmds"), recursive = T)
  dir.create(paste0(dir, "/ITEWR/Data"))
  dir.create(paste0(dir, "/ITEWR/Rcodes"))

  # download materials
  get_repo_files(folder = "master", type = ".Rmd", out = paste0(dir, "/ITEWR/Rmds/"))
  get_repo_files(folder = "master/data", out = paste0(dir, "/ITEWR/data/"))

  # tangle R code from Rmd files
  squeeze_rmds(dir = paste0(dir, "/ITEWR/Rmds/") ,
                            numbered = T,
                            prefix = "Chapter_",
                            out = paste0(dir, "/ITEWR/Rcodes/"))

}

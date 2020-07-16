#' load_taxonomies
#'
#' @param destination
#'
#' @return
#' @export
#'
#' @examples
load_taxonomies <- function(destination) {
  taxonomies <- readr::read_csv(paste0(destination,"Taxonomies.csv"))
}


#' load_categories
#'
#' @param destination
#'
#' @return
#' @export
#'
#' @examples
load_categories <- function(destination) {
  categories <- readr::read_csv(paste0(destination,"Categories.csv"))
}

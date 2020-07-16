#' Title
#'
#' @param
#'
#' @return
#' @export
#'
#' @examples
previous_month <- function() {
    z <- as.Date(format(Sys.Date(), "%Y-%m-01")) - 1
    date <- format(z, "%Y-%m")
    return(date)
}

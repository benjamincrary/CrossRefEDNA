#' Title
#'
#' @param end_date
#' @param email
#'
#' @return
#' @export
#'
#' @examples
create_n_results_query <- function(end_date, email) {

  nquery <- paste0("https://api.crossref.org/v1.0/works?query=eDNA+Environmental_DNA&filter=from-pub-date:2010-01,until-pub-date:",
                   end_date,
                   ",type:journal-article&rows=0&mailto=",
                   email)

  return(nquery)

}



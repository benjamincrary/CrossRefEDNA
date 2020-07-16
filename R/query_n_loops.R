#' Title
#'
#' @param enddate
#' @param email
#'
#' @return
#' @export
#'
#' @examples
query_n_loops <- function(email) {

  #set end date to previous month from today
  enddate <- previous_month()

  #get initial query with nresutls (does not contain full results)
  nquery <- create_n_results_query(enddate, email)

  #query crossREF with initial query to get n results
  initial <- jsonlite::fromJSON(nquery)

  #count results
  nres <- initial$message$`total-results`

  #convert count to loops of 1000 results
  loops <- ceiling(nres/1000)

  return(loops)

}

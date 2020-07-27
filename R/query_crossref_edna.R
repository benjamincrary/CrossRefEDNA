#' query_crossref_edna
#'
#' @param destination
#' @param backup
#' @param email
#'
#' @return
#' @export
#'
#' @examples
query_crossref_edna <- function(input_destination, email ) {

  #find end date of query
  enddate <- previous_month()

  #set output locations
  backup_destination <- set_backup_destination(input_destination)

  #run query loop
  results <- query_crossref_loop(email)

  #save loop results
  #saveRDS(results,
  #        paste0(backup_destination, "eDNA_201001-", substr(enddate, 1,4), substr(enddate,6,7), ".rds"))

  return(results)
}

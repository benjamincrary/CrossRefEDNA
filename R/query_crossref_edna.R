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
query_crossref_edna <- function(destination, backup, email ) {

  #find end date of query
  enddate <- previous_month()

  #set output locations
  result_destination <- set_destination(destination)
  backup_destination <- set_backup_destination(backup)

  #run query loop
  results <- query_crossref_loop(email)

  #save loop results
  saveRDS(results,
          paste0(backup, "eDNA_201001-", substr(enddate, 1,4), substr(enddate,6,7), ".rds"))

  return(resutls)
}

#' update_edna_knowledge_center
#'
#' @param destination
#' @param backup
#' @param email
#'
#' @return
#' @export
#'
#' @examples
update_edna_knowledge_center <- function(destination, backup, email) {

  #1. query
  results <- query_crossref_edna(destination, backup, email)

  #2. filter
  filtered_edna <- filter_edna_results(results, destination)

  #3. bin
  bin_filtered_results(filtered_edna, destination)


}
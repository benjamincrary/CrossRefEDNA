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
update_edna_knowledge_center <- function(input_destination, data_location, email) {

  #1. query
  results <- query_crossref_edna(input_destination, email)

  #2. filter
  filtered_edna <- filter_edna_results(results, input_destination)

  #3. bin
  bin_filtered_results(filtered_edna, input_destination, data_location)

  #4. check and update inputs
  finalize_update(input_destination)

  #5. commit changes


}

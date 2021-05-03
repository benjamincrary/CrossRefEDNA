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

  #1. verify input_destination and data_location exist
  dir_status <- check_directories(input_destination, data_location)

  #2. query
  results <- query_crossref_edna(input_destination, email, dir_status)

  #3. filter
  filtered_edna <- filter_edna_results(results, input_destination)

  #4. bin
  bin_filtered_results(filtered_edna, input_destination, data_location)

  #5. check and update inputs
  finalize_update(input_destination)

  #6. commit changes (not yet incorporated, need client input/feedback)


}

#' save_filtered_results
#'
#' @param input_location
#' @param filtered_results
#'
#' @return
#' @export
#'
#' @examples
save_filtered_results <- function(filtered_results, input_destination) {

  #save as RDS file
  saveRDS(filtered_results, paste0(input_destination, "filtered_eDNA_results.rds" ))

}

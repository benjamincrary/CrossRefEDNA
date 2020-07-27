#' filter_edna_results
#'
#' @param results
#'
#' @return
#' @export
#'
#' @examples
filter_edna_results <- function(results, input_destination) {

  #get date
  enddate <- previous_month()

  #Filter out false hits
  filtered_results <- subset(results, grepl("environmental DNA| eDNA|Environmental DNA", title) &
                              !grepl("extracellular|Extracellular|biofilm|Biofilm", title) |
                              grepl("environmental DNA| eDNA|Environmental DNA", subject) &
                              !grepl("extracellular|Extracellular|biofilm|Biofilm", title) |
                              grepl("environmental DNA| eDNA|Environmental DNA", subtitle) &
                              !grepl("extracellular|Extracellular|biofilm|Biofilm", title) |
                              grepl("environmental DNA| eDNA|Environmental DNA", abstract) &
                              !grepl("extracellular|Extracellular|biofilm|Biofilm", title))

  saveRDS(filtered_results, paste0(input_destination, "filtered_eDNA_resuls.rds" ))
  #saveRDS(filtered_results, paste0(destination, "filteredResults_toBinning.rds"))


  return(filtered_results)


}

#' filter_edna_results
#'
#' @param results
#'
#' @return
#' @export
#'
#' @examples
filter_edna_results <- function(results, destination) {

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

  saveRDS(filtered_results, paste0(destination, "filtered_eDNA_201001-", substr(enddate, 1,4), substr(enddate,6,7),".rds" ))
  saveRDS(filtered_results, paste0(destination, "filteredResults_toBinning.rds"))


  return(filtered_results)


}

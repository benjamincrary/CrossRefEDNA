#' filter_edna_results
#'
#' @param results
#'
#' @return
#' @export
#'
#' @examples
filter_edna_results <- function(results) {


  #get date
  enddate <- previous_month()


  #add columns if missing
# results <- results %>%
#    dplyr::mutate(title = ifelse("title" %in% colnames(results), title, list())) %>%
#    dplyr::mutate(subtitle = ifelse("subtitle" %in% colnames(results), subtitle, list())) %>%
#    dplyr::mutate(subject = ifelse("subject" %in% colnames(results), subject, list())) %>%
#    dplyr::mutate(abstract = ifelse("abstract" %in% colnames(results), abstract, list()))


  #Filter out false hits
  filtered_results <- subset(results, grepl("environmental DNA| eDNA|Environmental DNA", title) &
                              !grepl("extracellular|Extracellular|biofilm|Biofilm", title) |
                              grepl("environmental DNA| eDNA|Environmental DNA", subject) &
                              !grepl("extracellular|Extracellular|biofilm|Biofilm", title) |
                              grepl("environmental DNA| eDNA|Environmental DNA", subtitle) &
                              !grepl("extracellular|Extracellular|biofilm|Biofilm", title) |
                              grepl("environmental DNA| eDNA|Environmental DNA", abstract) &
                              !grepl("extracellular|Extracellular|biofilm|Biofilm", title))



  return(filtered_results)


}

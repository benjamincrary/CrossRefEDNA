#' bin_filtered_results
#'
#' @param filtered_edna
#' @param destination
#'
#' @return
#' @export
#'
#' @examples
bin_filtered_results <- function(filtered_edna, destination) {

  taxonomies <- load_taxonomies(destination)
  categories <- load_categories(destination)

  # bin and consolidate
  taxResults <- listConstruct(filtered_edna, taxonomies) %>%
    dplyr::mutate(Other=ifelse(rowSums(.==F) == ncol(taxonomies), TRUE, FALSE))
  catResults <- listConstruct(filtered_edna, categories) %>%
    dplyr::mutate(Other=ifelse(rowSums(.==F) == ncol(categories), TRUE, FALSE))


  taxSummary <- filtered_edna %>%
    dplyr::select(one_of("title", "author", "container-title", "link", "issued", "is-referenced-by-count", "issued.date-parts")) %>%
    dplyr::mutate(date = extractDateFromParts(`issued.date-parts`)) %>%
    dplyr::select(one_of("title", "author", "container-title", "link", "is-referenced-by-count", "date")) %>%
    dplyr::bind_cols(., taxResults) %>%
    tidyr::gather("taxonomy", "tax", 7:21) %>%
    dplyr::filter(tax=="TRUE") %>%
    dplyr::mutate_all(dplyr::funs(nullToNA(.))) %>%
    dplyr::mutate(leadAuthor=extractLeadAuthor(author)) %>%
    dplyr::mutate(URL = unlist(purrr::map(link, extractURL))) %>%
    dplyr::mutate(`container-title` = purrr::map(`container-title`,1)) %>%
    dplyr::mutate_at(dplyr::vars(title, `container-title`, `is-referenced-by-count`), dplyr::funs(unlist(.)))  %>%
    dplyr::select(-tax, -author, -link)


  catSummary <- filtered_edna %>%
    dplyr::select(one_of("title", "author", "container-title", "link", "issued", "is-referenced-by-count", "issued.date-parts")) %>%
    dplyr::mutate(date = extractDateFromParts(`issued.date-parts`)) %>%
    dplyr::select(one_of("title", "author", "container-title", "link", "is-referenced-by-count", "date")) %>%
    dplyr::bind_cols(., catResults) %>%
    tidyr::gather("category", "cat", 7:15) %>%
    dplyr::filter(cat=="TRUE") %>%
    dplyr::mutate_all(dplyr::funs(nullToNA(.))) %>%
    dplyr::mutate(leadAuthor=extractLeadAuthor(author)) %>%
    dplyr::mutate(URL = unlist(purrr::map(link, extractURL))) %>%
    dplyr::mutate(`container-title` = purrr::map(`container-title`,1)) %>%
    dplyr::mutate_at(dplyr::vars(title, `container-title`, `is-referenced-by-count`), dplyr::funs(unlist(.))) %>%
    dplyr::select(-cat, -author,-link)


  ##### Format for input into Shiny App

  Summary <- dplyr::full_join(catSummary,taxSummary) %>%
    dplyr::mutate_at(dplyr::vars(taxonomy), stringr::str_replace, pattern="Marine\\.Fish..", replace="Marine Fish") %>%
    dplyr::mutate_at(dplyr::vars(taxonomy, category), stringr::str_replace, pattern="\\.", replace=" ") %>%
    dplyr::mutate_at(dplyr::vars(taxonomy, category), stringr::str_replace, pattern="\\.and\\.", replace=" and ") %>%
    dplyr::mutate_at(dplyr::vars(taxonomy, category), stringr::str_replace, pattern="Field\\.", replace="Field ") %>%
    dplyr::mutate_at(dplyr::vars(taxonomy, category), dplyr::funs(replace(.,is.na(.), "Other"))) %>%
    dplyr::mutate_if(is.character, dplyr::funs(stringr::str_replace(.,pattern="[\r\n]", replace=""))) %>%
    dplyr::rename(TaxonomyBin=taxonomy, CategoryBin=category, Journal = `container-title`, Title=title) %>%
    dplyr::rename(`Cited by` = `is-referenced-by-count`) %>%
    dplyr::mutate_at(dplyr::vars(date), as.Date, origin="1970-01-01") %>%
    dplyr::mutate(PublicationYear = lubridate::year(date), PublicationMonth = lubridate::month(date), Access="subscription")

  AllPubs <- Summary %>%
    dplyr::select(Title, leadAuthor, Journal, date, `Cited by`, leadAuthor, URL) %>%
    dplyr::rename(`Publication Date` = date, `Lead Author`=leadAuthor) %>%
    dplyr::distinct()


  saveRDS(Summary,paste0(destination, "Input.RDS"))
  saveRDS(AllPubs,paste0(destination, "AllPubs.RDS"))


}

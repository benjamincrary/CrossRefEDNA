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
    mutate(Other=ifelse(rowSums(.==F) == ncol(taxonomies), TRUE, FALSE))
  catResults <- listConstruct(filtered_edna, categories) %>%
    mutate(Other=ifelse(rowSums(.==F) == ncol(categories), TRUE, FALSE))

  taxSummary <- filtered_edna %>%
    select(one_of("title", "author", "container-title", "link", "issued", "is-referenced-by-count", "issued.date-parts")) %>%
    mutate(date = extractDateList(issued)) %>%
    select(one_of("title", "author", "container-title", "link", "is-referenced-by-count", "date")) %>%
    bind_cols(., taxResults) %>%
    tidyr::gather("taxonomy", "tax", 7:21) %>%
    filter(tax=="TRUE") %>%
    mutate_all(funs(nullToNA(.))) %>%
    mutate(leadAuthor=extractLeadAuthor(author)) %>%
    mutate(URL = unlist(map(link, extractURL))) %>%
    mutate_at(vars(title, `container-title`, `is-referenced-by-count`), funs(unlist(.))) %>%
    select(-tax, -author, -link)


  catSummary <- filtered_edna %>%
    select(one_of("title", "author", "container-title", "link", "issued", "is-referenced-by-count", "issued.date-parts")) %>%
    mutate(date = extractDateList(issued)) %>%
    select(one_of("title", "author", "container-title", "link", "is-referenced-by-count", "date")) %>%
    bind_cols(., catResults) %>%
    tidyr::gather("category", "cat", 7:15) %>%
    filter(cat=="TRUE") %>%
    mutate_all(funs(nullToNA(.))) %>%
    mutate(leadAuthor=extractLeadAuthor(author)) %>%
    mutate(URL = unlist(map(link, extractURL))) %>%
    mutate_at(vars(title, `container-title`, `is-referenced-by-count`), funs(unlist(.))) %>%
    select(-cat, -author,-link)


  ##### Format for input into Shiny App

  Summary <- full_join(catSummary,taxSummary) %>%
    mutate_at(vars(taxonomy), str_replace, pattern="Marine\\.Fish..", replace="Marine Fish") %>%
    mutate_at(vars(taxonomy, category), str_replace, pattern="\\.", replace=" ") %>%
    mutate_at(vars(taxonomy, category), str_replace, pattern="\\.and\\.", replace=" and ") %>%
    mutate_at(vars(taxonomy, category), str_replace, pattern="Field\\.", replace="Field ") %>%
    mutate_at(vars(taxonomy, category), funs(replace(.,is.na(.), "Other"))) %>%
    mutate_if(is.character, funs(str_replace(.,pattern="[\r\n]", replace=""))) %>%
    rename(TaxonomyBin=taxonomy, CategoryBin=category, Journal = `container-title`, Title=title) %>%
    rename(`Cited by` = `is-referenced-by-count`) %>%
    mutate_at(vars(date), as.Date, origin="1970-01-01") %>%
    mutate(PublicationYear = year(date), PublicationMonth = month(date), Access="subscription")

  AllPubs <- Summary %>%
    select(Title, leadAuthor, Journal, date, `Cited by`, leadAuthor, URL) %>%
    rename(`Publication Date` = date, `Lead Author`=leadAuthor) %>%
    distinct()


  saveRDS(Summary,paste0(destination, "Input.RDS"))
  saveRDS(AllPubs,paste0(destination, "AllPubs.RDS"))


}

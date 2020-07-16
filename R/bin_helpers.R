#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
listAuthor <- function(x){
  xx <- as.data.frame(x)
  xxx <- paste(xx$family[1], xx$given[1], sep=", ")
  xxxx <- paste0(xxx, " ")
  return(xxxx)
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
extractLeadAuthor <- function(x) {
  xx <- purrr::map(x, listAuthor)
  xxx <- unlist(xx)
  return(xxx)
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
extractDate <- function(x) {
  year <- x[1]
  month <- x[2]
  day <- x[3]
  d <- paste(month,day,year, sep="/")
  as.Date(d, format="%m/%d/%Y",origin="1970-01-01")
}


#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
extractDateList <- function(x) {
  zz <- as.data.frame(x)
  zzz <- zz %>% tidyr::separate(`date-parts`, c("x", "year", "month", "day","xx")) %>%
    dplyr::mutate(day = ifelse(day == "", 1, day)) %>%
    dplyr::mutate(d = paste(month, day, year, sep="/")) %>%
    dplyr::mutate(d = as.Date(d, format="%m/%d/%Y",origin="1970-01-01"))
  zzz$d
}

extractDateFromParts <- function(partslist) {
  z <- partslist
  z2 <- purrr::map(z, function(x) {append(x,c(1,1))})
  z3 <- purrr::map(z2, function(x) {head(x, n=3)})

  date <- lubridate::ymd(z3)

  return(date)
}


#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
extractURL <- function(x) {
  xx <- x[[1]][1]
}


#' Title
#'
#' @param results
#' @param binset
#'
#' @return
#' @export
#'
#' @examples
listConstruct <- function(results, binset) {
  out <- binset %>% purrr::map(
    function(x) {
      results %>%
        dplyr::select(title, subtitle, abstract, subject) %>%
        dplyr::mutate_all(dplyr::funs(grepl(paste0("\\b",na.omit(x), "\\b", collapse='|'), ., ignore.case=TRUE))) %>%
        purrr::reduce(`|`)
    }
  )
  summary <- as.data.frame(out)
  return(summary)
}



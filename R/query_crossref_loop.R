#' Title
#'
#' @param email
#'
#' @return
#' @export
#'
#' @examples
query_crossref_loop <- function(email) {

  q1 <- build_cr_loop_one(email)
  q2 <- build_cr_loop_n(email)

  #set up loop
  results <- NULL
  loops <- query_n_loops(email)
  loopseq <- seq(1:loops)

  for (i in loopseq) {
    if(i == 1) {
      x <-  fromJSON(q2, flatten=TRUE)
      curs <- x$message$`next-cursor`
      curs <- gsub("\\+", "%2B", curs)
      results <- rbind_pages(list(results, x$message$items))
    } else {
      x <-  fromJSON(paste0(q3, curs), flatten=TRUE)
      curs <- x$message$`next-cursor`
      curs <- gsub("\\+", "%2B", curs)
      results <- rbind_pages(list(results, x$message$items))
      print(i)
    }
  }

  return(results)
}

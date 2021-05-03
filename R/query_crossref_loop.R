#' Title
#'
#' @param email
#'
#' @return
#' @export
#'
#' @examples
query_crossref_loop <- function(email) {

  print("Building query...")
  q1 <- build_cr_loop_one(email)
  q2 <- build_cr_loop_n(email)

  #set up loop
  results <- NULL
  loops <- query_n_loops(email)
  loops <- 3 #dev option only
  loopseq <- seq(1:loops)

  print(paste0(loops, " total loops. Starting loop 1..."))

  for (i in loopseq) {
    if(i == 1) {
      x <-  jsonlite:::fromJSON(q1, flatten=TRUE)
      curs <- x$message$`next-cursor`
      curs <- gsub("\\+", "%2B", curs)
      results <- jsonlite::rbind_pages(list(results, x$message$items))
      print(paste0(i, "/", loops, " complete"))
    } else {
      tryCatch({
        x <-  jsonlite::fromJSON(paste0(q2, curs), flatten=TRUE)
        curs <- x$message$`next-cursor`
        curs <- gsub("\\+", "%2B", curs)
        results <- jsonlite::rbind_pages(list(results, x$message$items))
        print(paste0(i, "/", loops, " complete"))
      }, error=function(e){cat("ERROR :",conditionMessage(e), "on loop ", i, "\n")})
    }
  }

  return(results)
}

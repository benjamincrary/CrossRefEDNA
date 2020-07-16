#' build_cr_loop_one
#'
#' @param email
#'
#' @return
#' @export
#'
#' @examples
build_cr_loop_one <- function(email) {

  enddate <- previous_month()

  #query one will set the cursor for the looping process
  query_one <- paste0("https://api.crossref.org/v1.0/works?query=eDNA+Environmental_DNA&filter=from-pub-date:2010-01,until-pub-date:",
               enddate,
               ",type:journal-article&mailto=",
               email,
               "&rows=1000&cursor=*")

  return(query_one)

}


#' build_cr_loop_n
#'
#' @param email
#'
#' @return
#' @export
#'
#' @examples
build_cr_loop_n <- function(email) {

  enddate <- previous_month()

  #query_n will require a cursor from loop one or loop n-1 (see 'function')
  query_n <- paste0("https://api.crossref.org/v1.0/works?query=eDNA+Environmental_DNA&filter=from-pub-date:2010-01,until-pub-date:",
                    enddate,
                    ",type:journal-article&mailto=",
                    email,
                    "&rows=1000&cursor=")

  return(query_n)

}

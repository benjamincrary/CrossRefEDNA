#' check updated inputs
#'
#' @param destination
#'
#' @return
#' @export
#'
#' @examples
check_updated_inputs <- function(destination) {

  input <- readRDS(paste0(destination, "Input.RDS"))
  input_backup <- readRDS(paste0(destination, "Input_backup.RDS"))

  row_status <- ifelse(nrow(input) <= nrow(input_backup), "fail", "pass")
  date_status <- ifelse(max(input$date) <= max(input_backup$date), "fail", "pass")

  combined_status <- data.frame(status = c(row_status, date_status))
  input_status <- ifelse(sum(combined_status$status == "fail") > 0, "fail", "pass")

  return(input_status)

}

#' check updated publist
#'
#' @param destination
#'
#' @return
#' @export
#'
#' @examples
check_updated_publist <- function(destination) {

  input <- readRDS(paste0(destination, "AllPubs.RDS"))
  input_backup <- readRDS(paste0(destination, "AllPubs_backup.RDS"))

  row_status <- ifelse(nrow(input) <= nrow(input_backup), "fail", "pass")
  date_status <- ifelse(max(input$`Publication Date`) <= max(input_backup$`Publication Date`), "fail", "pass")

  combined_status <- data.frame(status = c(row_status, date_status))
  input_status <- ifelse(sum(combined_status$status == "fail") > 0, "fail", "pass")

  return(input_status)

}




#' check updates
#'
#' @param destination
#'
#' @return
#' @export
#'
#' @examples
check_updates <- function(destination) {

  i <- check_updated_inputs(destination)
  p <- check_updated_publist(destination)

  combined_status <- data.frame(status = c(i, p))

  update_status <- ifelse(sum(combined_status$status == "fail") > 0, "fail", "pass")

  return(update_status)

}


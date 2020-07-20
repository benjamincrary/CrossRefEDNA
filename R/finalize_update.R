#' finalize update
#'
#' @param destination
#'
#' @return
#' @export
#'
#' @examples
finalize_update <- function(destination) {

  status <- check_updates(destination)


  if(status == "fail") {
    input_backup <- readRDS(paste0(destination, "Input_backup.RDS"))
    pub_backup <- readRDS(paste0(destination, "AllPub_backup.RDS"))

    saveRDS(input_backup, paste0(destination, "Input.RDS"))
    saveRDS(pub_backup, paste0(destination, "AllPub.RDS"))
  }

  log <- read.csv(paste0(destination, "UpdateLog.csv"))
  new_to_log <- data.frame(Sys.Date = Sys.Date(), Status.Check = status)
  log <- rbind(log, new_to_log)
  write.csv(log, paste0(destination, "UpdateLog.csv"))

}

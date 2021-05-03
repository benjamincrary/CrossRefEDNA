#' finalize update
#'
#' @param destination
#'
#' @return
#' @export
#'
#' @examples
finalize_update <- function(input_destination) {

  status <- check_updates(input_destination)


  if(status == "fail") {
    input_backup <- readRDS(paste0(input_destination, "Input_backup.RDS"))
    pub_backup <- readRDS(paste0(input_destination, "AllPubs_backup.RDS"))

    #save most recent builds to failed subdirectory
    saveRDS(readRDS(paste0(input_destination, "Input.RDS")), paste0(input_destination,"failed/","MostRecent_Input_Build_FailedStatus.RDS"))
    saveRDS(readRDS(paste0(input_destination, "AllPubs.RDS")), paste0(input_destination, "failed/","MostRecent_AllPub_Build_FailedStatus.RDS"))

    #rewrite inputs/allpubs with backups
    saveRDS(input_backup, paste0(input_destination, "Input.RDS"))
    saveRDS(pub_backup, paste0(input_destination, "AllPubs.RDS"))

  } else if(status == "pass") {
    file.copy(paste0(input_destination, "Input.RDS"), paste0(input_destination, "Input_backup.RDS"), overwrite = TRUE)
    file.copy(paste0(input_destination, "AllPubs.RDS"), paste0(input_destination, "AllPubs_backup.RDS"), overwrite = TRUE)
  }

  log <- read.csv(paste0(input_destination, "UpdateLog.csv")) %>% dplyr::mutate(Sys.Date = as.Date(Sys.Date, format = "%Y-%m-%d"))
  new_to_log <- data.frame(Sys.Date = Sys.Date(), Status.Check = status)
  log <- rbind(log, new_to_log)
  write.csv(log, paste0(input_destination, "UpdateLog.csv"), quote=F, row.names=F)

}

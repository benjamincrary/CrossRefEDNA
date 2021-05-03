#' check_directories
#'
#' @param input_location
#' @param data_location
#'
#' @return
#' @export
#'
#' @examples
check_directories <- function(input_location, data_location) {

  input_status <- dir.exists(input_location)
  data_status <- dir.exists(data_location)

  dir_status <- if(input_status[1] == FALSE | data_status == FALSE) {
    "Error"
  } else {
    "Okay"
  }

  if(dir_status == "Error") {
    stop("One or both provided paths do not exist_check")
  }


}

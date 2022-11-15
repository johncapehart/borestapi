#' @importFrom lubridate now parse_date_time2
#' @importFrom stringr str_detect str_extract_all str_length str_replace str_split str_sub
#' @importFrom dplyr mutate bind_rows as_tibble arrange
#' @importFrom readr read_file_raw read_delim
#'
utils::globalVariables(c("SI_ID", "SI_UPDATE_TS"))
utils::globalVariables(c("name", "id", "folder"))
utils::globalVariables(c("serverkey","usernamekey","serverkey","token"))
#utils::globalVariables(c(".", "Date", "pdate"))


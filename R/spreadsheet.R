#---------------------------

#' POST a new spreadsheet to BO folder
#'
#' @param conn Connection reference
#' @param filename Name for spreasheet file
#' @param parent_folder Numeric id of the folder
#' @param filepath Path to spreadsheet file (including file name)
#' @param format HTTP body format to use
post_bo_spreadsheet <- function(conn, filename, parent_folder, filepath = filename, format = 'json') {
  request <- check_bo_connection(conn)
  mimeType <- 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
  attachmentInfos <- list("spreadsheet" = list("name" = filename, "folderId" = parent_folder)) %>% toJSON()
  request %<>% httr2::req_body_multipart(attachmentContent = curl::form_file(filepath, type = mimeType),
                                 attachmentInfos = curl::form_data(attachmentInfos, type='application/json'))
  request %<>% httr2::req_url_path_append("raylight/v1/spreadsheets") %>%
    httr2::req_headers("Content-Type"="multipart/form-data")
  response <- httr2::req_perform(request)
  report_request_result(request, response, paste("POST upload of file", filename, parent_folder), "post_bo_spreadsheet", 69)
  return(httr2::resp_body_json(response))
}

#' PUT an existing spreadsheet to BO folder
#'
#' @param conn Connection reference
#' @param filename Name for spreasheet file
#' @param sheet_id Numeric id of spreadsheet document
#' @param filepath Path to spreadsheet file (including file name)
put_bo_spreadsheet <- function(conn, filename, sheet_id, filepath = filename) {
  request <- check_bo_connection(conn)
  mimeType <- 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
  request %<>% httr2::req_body_file(filepath, mimeType) %>%
    httr2::req_headers("Content-Type" = mimeType)
  request %<>% httr2::req_url_path_append("/raylight/v1/spreadsheets", sheet_id)
  request %<>% httr2::req_method("PUT")
  response <- httr2::req_perform(request)
  report_request_result(request, response, paste("PUT upload of file", filename, sheet_id,
    "put_bo_spreadsheet line 34"))
  return(httr2::resp_body_json(response))
}

#' Get spreadsheet properties from BO
#'
#' @param conn Connection reference
#' @param filename Name of the spreadsheet
#' @param parent_folder Numeric id of the folder
#'
#' @return List of properties
#' @export
get_bo_spreadsheet_details <- function(conn, filename, parent_folder) {
  request <- check_bo_connection(conn)
  sheet <- get_bo_item(conn, filename, parent_folder = parent_folder, kind = "Excel")

  if (nrow(sheet)) {
    request %<>% httr2::req_url_path_append("raylight/v1/spreadsheets", sheet$SI_ID)
    response <- httr2::req_perform(request)
    report_request_result( request, response, paste("GET file", filename, sheet$SI_ID), "get_bo_spreadsheet_details", 100)
    result <- httr2::resp_body_json(response) %>% flatten_scalars() %>% bind_rows()
    return(result)
  }
}

#' Delete a spreadsheet from BO
#'
#' @param conn Connection reference
#' @param filename Name of the spreadsheet
#' @param parent_folder Numeric id of the folder
#'
#' @return Response content
#' @export
delete_bo_spreadsheet <- function(conn, filename, parent_folder) {
  sheet <- get_bo_spreadsheet_details(conn, filename, parent_folder)
  if (nrow(sheet)) {
    request <- check_bo_connection(conn)
    request %<>% httr2::req_url_path_append("raylight/v1/spreadsheets", sheet$id)
    request %<>% httr2::req_method('DELETE')
    response <- httr2::req_perform(request)
    report_request_result( request, response, paste("GET file", filename, sheet$SI_ID), "get_bo_spreadsheet_details", 100)
    return(httr2::resp_body_json(response))
  }
}

#' Title
#'
#' @param conn Connection reference
#' @param filename Name of the spreadsheet
#' @param parent_folder Numeric id of the folder
#' @param filepath Path to spreadsheet file (including file name). Defaults to filename
#'
#' @return List of properties for spreadsheet
#' @export
upload_bo_spreadsheet <- function(conn, filename, parent_folder, filepath = filename) {
  request <- check_bo_connection(conn)
  logger::log_info(paste("Upload excel file", filename, ";tupload_bo_spreadsheet 141"))
  sheet <- get_bo_item(conn, filename, parent_folder = parent_folder, kind = "Excel")
  if (nrow(sheet)) {
    sheet <- put_bo_spreadsheet(conn, filename, sheet_id = sheet$SI_ID, filepath)
  } else {
    sheet <- post_bo_spreadsheet(conn, filename, parent_folder = parent_folder, filepath)
  }
  sheet
}

upload_bo_mtcars <- function(n = 100) {
  df <- mtcars
  filename <- Sys.getenv('BO_TEST_EXCEL_FILE_NAME')
  folder_id = Sys.getenv('BO_TEST_FOLDER_ID')
  library(openxlsx)
  # https://stackoverflow.com/questions/21502332/generating-random-dates
  rdate <- function(x, min = paste0(format(Sys.Date(), '%Y'), '-01-01'), max = paste0(format(Sys.Date(), '%Y'), '-12-31')) {
    dates <- sample(seq(as.Date(min), as.Date(max), by = "day"), x, replace = TRUE)
  }
  df$date = rdate(nrow(mtcars))
  df <- tibble::rownames_to_column(df, "model")
  df <- head(df, n)
  wb2 <- createWorkbook(mtcars)
  sheet <- wb2 %>% addWorksheet('Cars')
  writeDataTable(wb2, sheet, df)
  saveWorkbook(wb2, filename, overwrite = TRUE)
  upload_bo_spreadsheet(conn, filename, parent_folder = folder_id)
}

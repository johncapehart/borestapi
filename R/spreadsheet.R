#---------------------------

#' POST a new spreadsheet to BO folder
#'
#' @param conn Connection reference
#' @param filename Name for spreasheet file
#' @param parent_folder Numeric id of the folder
#' @param filepath Path to spreadsheet file (including file name)
#' @param format HTTP body format to use
#'
#' @returns Response details
#'
#' @export
#' @noRd
post_bo_spreadsheet <- function(conn, filename, parent_folder, filepath = filename, format = 'json', type = NULL) {
  request <- check_bo_connection(conn)
  if (is.null(type)) {
    content <- curl::form_file(filepath)
  } else {
    content <- curl::form_file(filepath, type)
  }
  attachmentInfos <- list("spreadsheet" = list("name" = filename, "folderId" = parent_folder)) %>% jsonlite::toJSON()
  request %<>% httr2::req_body_multipart(attachmentContent = content,
                                 attachmentInfos = curl::form_data(attachmentInfos, type='application/json'))
  request %<>% httr2::req_url_path_append("raylight", "v1", "spreadsheets")
  # httr2::req_dry_run(request)
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
#'
#' @returns Response details
#'
#' @export
#' @noRd
put_bo_spreadsheet <- function(conn, filename, sheet_id, filepath = filename, type = NULL) {
  request <- check_bo_connection(conn)
  if (is.null(type)) {
    request %<>% httr2::req_body_file(filepath)
  } else {
    request %<>% httr2::req_body_file(filepath, type)
  }
  request %<>% httr2::req_url_path_append("raylight", "v1", "spreadsheets", sheet_id)
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
  sheet <- get_bo_item(conn, filename, parent_folder = parent_folder)
  if (nrow(sheet)) {
    request %<>% httr2::req_url_path_append("raylight", "v1", "spreadsheets", sheet$SI_ID)
    response <- httr2::req_perform(request)
    report_request_result( request, response, paste("GET file", filename, sheet$SI_ID), "get_bo_spreadsheet_details", 100)
    result <- httr2::resp_body_json(response) %>% flatten_scalars() %>% dplyr::bind_rows()
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
#' @noRd
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

#' Upload a spreadsheet
#'
#' @param conn Connection reference
#' @param filename Name of the spreadsheet
#' @param parent_folder Numeric id of the folder
#' @param filepath Path to spreadsheet file (including file name). Defaults to filename
#'
#' @return List of properties for spreadsheet
#' @export
upload_bo_spreadsheet <- function(conn, filename, parent_folder, filepath = filename, type = NULL) {
  request <- check_bo_connection(conn)
  logger::log_info(paste("Upload excel file", filename, ";tupload_bo_spreadsheet 141"))
  sheet <- get_bo_item(conn, filename, parent_folder = parent_folder, kind = "Excel")
  if (nrow(sheet)) {
    sheet <- put_bo_spreadsheet(conn, filename, sheet_id = sheet$SI_ID, filepath, type = type)
  } else {
    sheet <- post_bo_spreadsheet(conn, filename, parent_folder = parent_folder, filepath, type =type)
  }
  sheet
}

#' @title Upload a dataframe
#'
#' @param conn Connection reference
#' @param df Dataframe
#' @param filename Name of uploaded file
#' @param parent_folder Parent folder to upload file to
#' @param sheetname Name of the sheet
#' @param type of file (Optional, defauts to extension of filename)
#'
#' @return spreadsheat details
#' @export
upload_bo_dataframe <- function(conn, df, filename, parent_folder, sheetname = 'Sheet 1', type = NULL) {
  if (stringr::str_detect(filename, '.xlsx$') || (!is.null(type) && (type == 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet' || type == '.xlsx'))) {
    type <- 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
    require(openxlsx)
    wb2 <- openxlsx::createWorkbook()
    sheet <- wb2 %>% openxlsx::addWorksheet(sheetname)
    openxlsx::writeDataTable(wb2, sheet, df)
    openxlsx::saveWorkbook(wb2, filename, overwrite = TRUE)
    result <- upload_bo_spreadsheet(conn, filename, parent_folder = parent_folder, filepath = filename, type= type)
  } else if (stringr::str_detect(filename, '.xls$') || (!is.null(type) && (type == 'application/vnd.ms-excel' || type == '.xls'))) {
    type <- 'application/vnd.ms-excel'
    require(WriteXLS)
    WriteXLS::WriteXLS(df, ExcelFileName=filename, SheetNames = sheetname)
    result <- upload_bo_spreadsheet(conn, filename, parent_folder = parent_folder, filepath = filename, type= type)
  } else if (stringr::str_detect(filename, '.csv$') || (!is.null(type) && (type == 'text/csv' || type == '.csv'))) {
    type <- 'text/csv'
    write.table(df, filename, sep = ',')
    result <- upload_agnostic_document(conn, filename, parent_folder = parent_folder, filepath = filename, type= type)
  }
  file.remove(filename)
  return(result)
}

#' Upload a document of any type
#'
#' WIP
#'
#' @param conn Connection reference
#' @param filename Name of file
#' @param parent_folder Parent folder to upload file to
#' @param type mime type (optional)
#'
#' @return result of upload
upload_bo_agnostic_document <- function(conn, filename, parent_folder, filepath = filename, type = NULL) {
  request <- check_bo_connection(conn)
  item <- get_bo_item(conn, filename, parent_folder = parent_folder)
  if (nrow(item)) {
    # not sure what to do if document exists
  }
  if (is.null(type)) {
    content <- curl::form_file(filepath)
  } else {
    content <- curl::form_file(filepath, type)
  }
  request %<>% httr2::req_body_multipart(attachmentContent = content)
  request %<>% httr2::req_url_path_append("infostore", 'folder', parent_folder, 'file') %>%
    httr2::req_headers('Accept' = '*.*')
  # biprws/infostore/folder/<folder_id>/file
  # httr2::req_dry_run(request)
  response <- httr2::req_perform(request)
  response$headers$`Content-Type`<-'text/xml' # force the response content type
  report_request_result(request, response, paste("POST upload of file", filename, parent_folder), "upload_agnostic_document", 69)

  return(httr2::resp_body_string(response))
}

#' Get children of a folder
#'
#' @param conn Connection reference
#' @param parent_folder id of parent folder
#'
#' @return tibble with chilren properties
#'
#' @export
get_bo_folder_children <- function(conn, parent_folder) {
  request <- check_bo_connection(conn)
  # /biprws/v1/folders/3944223/children
  request %<>% httr2::req_url_path_append("v1", 'folders', Sys.getenv('BO_TEST_FOLDER_ID'), 'children') %>%
    httr2::req_headers('Accept' = 'application/json')
  # httr2::req_dry_run(request)
  response <- httr2::req_perform(request)
  report_request_result(request, response, paste(";t upload_agnostic_document", 69))
  result <- httr2::resp_body_json(response)$entries %>% bind_bo_query_results_to_tibble()
  result
}

#---------------------------

get_bo_spreadsheet_attachmentInfos <- function(filename, parent_folder, format = 'json') {
  #filename = gsub('.xlsx','',filename)
  if (format == 'json') {
    json <- list("spreadsheet" = list("name" = filename, "folderId" = parent_folder)) %>% listToJSON()
    write(json, ".attachmentInfos")
    .body0 <-
      upload_file(".attachmentInfos", type = "application/json")
  } else {
    xml <-
      paste0(
        '<spreadsheet><name>',
        filename,
        '</name><folderId>',
        parent_folder,
        '</folderId></spreadsheet>'
      )
    write(xml, ".attachmentInfos")
    .body0 <- upload_file(".attachmentInfos", type = "application/xml")
  }
  return(.body0)
}

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
  url <- paste0(request$url, "/raylight/v1/spreadsheets")
  request$headers = add_bo_headers(request$headers)
  .body0 <- get_bo_spreadsheet_attachmentInfos(filename, parent_folder, format = format)
  .body1 <- upload_file(filepath, type = mimeType)
  .body <- list(attachmentInfos = .body0, attachmentContent = .body1)
  #url <- request$url <- http://localhost:8888' # for test capture by nc -kl 8888
  response <-POST(url, body = .body, request)
  if (file.exists(".attachmentInfos")) {
    file.remove(".attachmentInfos")
  }
  report_request_result(request, response, paste("POST upload of file", filename, parent_folder), "post_bo_spreadsheet", 69)
  return(content(response))
}

#' PUT an existing spreadsheet to BO folder
#'
#' @param conn Connection reference
#' @param filename Name for spreasheet file
#' @param sheet_id Numeric id of spreadsheet document
#' @param filepath Path to spreadsheet file (including file name)
put_bo_spreadsheet <- function(conn, filename, sheet_id, filepath = filename) {
  request <- check_bo_connection(conn)
  request$headers = add_bo_headers(request$headers)
  .body <- upload_file(filepath)
  url <- paste0(request$url, "/raylight/v1/spreadsheets/", sheet_id)
  response <-
    PUT(url,
        body = list(attachmentContent = .body),
        encode = "multipart",
        request)
  report_request_result(
    request,
    response,
    paste("PUT upload of file", filename, sheet_id),
    "put_bo_spreadsheet",
    334
  )
  content(response)
}

#' Get spreadsheet properties from BO
#'
#' @param conn Connection reference
#' @param filename Name of the spreadsheet
#' @param parent_folder Numeric id of the folder
#'
#' @return List of properties
#' @export
get_bo_spreadsheet <- function(conn, filename, parent_folder) {
  request <- check_bo_connection(conn)
  sheet <- get_bo_item(conn, filename, parent_folder = parent_folder, kind = "Excel")

  request$headers = add_bo_headers(request$headers)
  if (nrow(sheet)) {
    url <- paste0(request$url, "/raylight/v1/spreadsheets/", sheet$SI_ID)
    response <- GET(url, request)
    report_request_result( request, response, paste("GET file", filename, sheet$SI_ID), "get_bo_spreadsheet", 100)
    content(response)
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
  request <- check_bo_connection(conn)
  sheet <- get_bo_item(conn, filename, parent_folder = parent_folder, kind = "Excel")

  request$headers = add_bo_headers(request$headers)
  if (nrow(sheet) == 1) {
    url <- paste0(request$url, "/raylight/v1/spreadsheets/", sheet$SI_ID)
    response <- DELETE(url, request)
    report_request_result( request, response, paste("DELETE file", filename, sheet$SI_ID), "delete_bo_spreadsheet", 138)
    content(response)
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
  logger::log_info("Upload excel file",  filename, ";;upload_bo_spreadsheet 141", duration = 60)
  sheet <- get_bo_item(conn, filename, parent_folder = parent_folder, kind = "Excel")
  if (nrow(sheet)) {
    sheet <- put_bo_spreadsheet(conn, filename, sheet_id = sheet$SI_ID, filepath)
  } else {
    sheet <- post_bo_spreadsheet(conn, filename, parent_folder = parent_folder, filepath)
  }
}

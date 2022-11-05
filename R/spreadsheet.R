#---------------------------

#' POST a new spreadsheet to BO folder
#'
#' @param conn Connection reference
#' @param filepath Path to spreadsheet file (including name)
#' @param filename Name for spreasheet file
#' @param folder_id Numeric id of the folder
#' @param format HTTP body format to use
#'
#' @return
POST_bo_spreadsheet_raw <- function(conn,  filepath, filename, folder_id, format = 'json') {
  request <- check_bo_connection(conn)
  mimeType <- 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
  mimeType <- 'application/vnd.ms-excel'
  url <- paste0(request$url, "/raylight/v1/spreadsheets")
    boundary <- '------------------------dcaca6aa0076de4e'
  boundary2 <- paste0('--', boundary)
  request$headers[["Accept"]] <- "application/json"
  request$headers[["Accept-Encoding"]] <- "gzip, deflate"
  request$headers[["Content-Type"]] <- paste0("multipart/form-data; boundary=",boundary)
  fileData <- readBin(filepath, "raw")
  con = file("body.txt", "wb")
  writeBin(paste0(boundary2, "\r\n", "Content-Disposition: form-data; name=\"attachmentInfos\"\r\nContent-Type: application/json\r\n\r\n",
                  '{"spreadsheet":{"name":"',filename,'","folder_id":',folder,'}}\r\n',
                  boundary2,"\r\n",
                  "Content-Disposition: form-data; name=\"attachmentContent\"\r\n",
                  "Content-Type: application/vnd.openxmlformats-officedocument.spreadsheetml.sheet\r\n\r\n"), con=con)
  writeBin(fileData, con=con)
  writeBin(paste0(boundary2,"--"), con=con)
  close(con)
  con = file("body.txt", "rb")
  body = read_file_raw("body.txt")
  #url <- request$url <- http://localhost:8888' # for test capture by nc -kl 8888
  response <-POST(url, body = body, request)
  report_api_error(request, response, paste("POST upload of file", filename, folder_id), "POST_bo_spreadsheet_raw", 51)
  return(content(response))
}

get_bo_spreadsheet_attachmentInfos <- function(filename, folder_id, format = 'json') {
  #filename = gsub('.xlsx','',filename)
  if (format == 'json') {
    json <- list("spreadsheet" = list("name" = filename, "folder_id" = folder_id)) %>% listToJSON()
    write(json, ".attachmentInfos")
    .body0 <-
      upload_file(".attachmentInfos", type = "application/json")
  } else {
    xml <-
      paste0(
        '<spreadsheet><name>',
        filename,
        '</name><folder_id>',
        folder_id,
        '</folder_id></spreadsheet>'
      )
    write(xml, ".attachmentInfos")
    .body0 <- upload_file(".attachmentInfos", type = "application/xml")
  }
  return(.body0)
}

#' POST a new spreadsheet to BO folder
#'
#' @param conn Connection reference
#' @param filepath Path to spreadsheet file (including file name)
#' @param filename Name for spreasheet file
#' @param folder_id Numeric id of the folder
#' @param format HTTP body format to use
POST_bo_spreadsheet <- function(conn,  filepath, filename, folder_id, format = 'json') {
  request <- check_bo_connection(conn)
  mimeType <- 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
  #mimeType <- 'application/vnd.ms-excel'
  url <- paste0(request$url, "/raylight/v1/spreadsheets")
  request$headers = add_bo_headers(request$headers)
  .body0 <- get_bo_spreadsheet_attachmentInfos(filename, folder_id, format = format)
  .body1 <- upload_file(filepath, type = mimeType)
  .body <- list(attachmentInfos = .body0, attachmentContent = .body1)
  #url <- request$url <- http://localhost:8888' # for test capture by nc -kl 8888
  response <-POST(url, body = .body, request)
  if (file.exists(".attachmentInfos")) {
    file.remove(".attachmentInfos")
  }
  report_api_error(request, response, paste("POST upload of file", filename, folder_id), "POST_bo_spreadsheet", 69)
  return(content(response))
}

#' PUT an existing spreadsheet to BO folder
#'
#' @param conn Connection reference
#' @param filepath Path to spreadsheet file (including file name)
#' @param filename Name for spreasheet file
#' @param folder_id Numeric id of the folder
PUT_bo_spreadsheet <- function(conn, filepath, filename, sheetId) {
  request <- check_bo_connection(conn)
  request$headers = add_bo_headers(request$headers)
  .body <- upload_file(filepath)
  url <- paste0(request$url, "/raylight/v1/spreadsheets/", sheetId)
  response <-
    PUT(url,
        body = list(attachmentContent = .body),
        encode = "multipart",
        request)
  report_api_error(
    request,
    response,
    paste("PUT upload of file", filename, sheetId),
    "PUT_bo_spreadsheet",
    334
  )
  content(response)
}

#' Get spreadsheet properties from BO
#'
#' @param conn Connection reference
#' @param filename Name of the spreadsheet
#' @param folder Numeric id of the folder
#'
#' @return List of properties
#' @export
GET_bo_spreadsheet <- function(conn, filename, folder) {
  request <- check_bo_connection(conn)
  sheet <- get_bo_item_from_name(conn, filename, folder = folder, kind = "Excel")

  request$headers = add_bo_headers(request$headers)
  if (nrow(sheet)) {
    url <- paste0(request$url, "/raylight/v1/spreadsheets/", sheet$SI_ID)
    response <- GET(url, request)
    report_api_error( request, response, paste("GET file", filename, sheet$SI_ID), "GET_bo_spreadsheet", 100)
    content(response)
  }
}

#' Delete a spreadsheet from BO
#'
#' @param conn Connection reference
#' @param filename Name of the spreadsheet
#' @param folder Numeric id of the folder
#'
#' @return
#' @export
DELETE_bo_spreadsheet <- function(conn, filename, folder) {
  request <- check_bo_connection(conn)
  sheet <- get_bo_item_from_name(conn, filename, folder = folder, kind = "Excel")

  request$headers = add_bo_headers(request$headers)
  if (nrow(sheet) == 1) {
    url <- paste0(request$url, "/raylight/v1/spreadsheets/", sheet$SI_ID)
    response <- DELETE(url, request)
    report_api_error( request, response, paste("DELETE file", filename, sheet$SI_ID), "DELETE_bo_spreadsheet", 138)
    content(response)
  }
}

#' Title
#'
#' @param conn Connection reference
#' @param filepath Path to spreadsheet file (including file name)
#' @param filename Name of the spreadsheet
#' @param folder Numeric id of the folder
#'
#' @return List of properties for spreadsheet
#' @export
upload_bo_spreadsheet <- function(conn, filepath, filename, folder) {
  request <- check_bo_connection(conn)
  mycat(";Upload excel file",  filename, ";upload_bo_spreadsheet 141", duration = 60)
  sheet <- get_bo_item_from_name(conn, filename, folder = folder, kind = "Excel")
  if (nrow(sheet)) {
    sheet <- PUT_bo_spreadsheet(conn, filepath, filename, sheetId = sheet$SI_ID)
  } else {
    sheet <- POST_bo_spreadsheet(conn, filepath, filename, folder_id = folder)
  }
}

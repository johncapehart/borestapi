#' @importFrom purrr quietly pluck
#' @importFrom tibble tibble enframe as_tibble is_tibble
#' @importFrom tidyr pivot_wider
#' @include api-utils.R
#'
#'
#'

testNumericArgument <- function(x) {
  quietly(function(x)!is.na(as.numeric(x)))(x)$result
}

#' Get data from a document provider
#'
#' @param conn Connection reference
#' @param document Document as numeric id or tibble of properties
#' @param provider_id Numeric id of the provider
#' @param ... Parameters for read_delim
#'
#' @return
#' @export

get_bo_document_data <- function(conn, document, provider_id, ...) {
  request <- check_bo_connection(conn)
  document_id <- get_bo_item_id(document)
  request$headers[["Accept"]] <- "text/plain"
  url <- paste(url, provider_id, "flows/0", sep = "/")
  response <- GET(url = url, request)
  df <- content(response, as = "text", encoding = "UTF-8") %>% read_delim(delim = ";", ...)
  report_api_error(request, response, paste("Get document data rows", nrow(df), "columns", ncol(df)), "get_bo_documentData", 246)
  df
}

#' Title
#'
#' @param conn Connection reference
#' @param document Document as numeric id or tibble of properties
#' @param save If true save the document before closing
#'
#' @return
#' @export
close_bo_document <- function(conn, document, save = FALSE) {
  request <- check_bo_connection(conn)
  document_id <- get_bo_item_id(document)
  url <- paste(request$url, "raylight/v1/documents", document_id, "occurrences", sep = "/")
  response <- GET(url, request)
  report_api_error(request, response, paste(";GET occurances", document_id), "close_bo_document", 55)
  occurrance <- content(response)$occurrences$occurrence
  if (length(occurrance) == 0) {
    mycat("No occurences to close for", document_id)
    return()
  } else if (length(occurrance) > 1) {
    mycat(length(occurrance), "occurences to found for", document_id)
  }
  occurrance %<>% keep(function(x)x$state!='Unused') %>% head(1)
  if (length(occurrance) == 1) {
    occurrance %<>% unlist()
    url <- paste(url, occurrance[["id"]], sep = "/")
    if (save) {
      body <- NULL
    } else {
      body <- list("occurrence" = list(state = 'Unused')) %>% listToJSON()
    }
    response <- PUT(url, body = body, request)
    report_api_error(request, response, paste("Closing document", document$SI_Name, ";PUT occurance", occurrance[['state']]), "close_bo_document", 71)
    return_bo_response_content(response)
  }
}

#' Title
#'
#' @param conn Connection reference
#' @param filepath
#' @param filename
#' @param folder_id
#' @param server
#'
#' @return
#' @export
POST_bo_document <- function(conn, filepath, filename, folder_id, server) {
  request <- check_bo_connection(conn)
  .body <- upload_file(filepath)
  request$headers[["Accept"]] <- "*/*"
  request$headers[["Accept-Encoding"]] <- "gzip, deflate"
  request$headers[["Host"]] <- server
  request$headers[["Content-Type"]] <- "multipart/form-data"
  url <- paste0(request$url, "/infostore/folder/", folder_id, "/file")
  response <- POST(url, body = list(y = .body), request)
  report_api_error(request, response, paste(";POST", filepath,filename), "POST_bo_document", 83)
}

#' Title
#'
#' @param conn Connection reference
#' @param filepath
#' @param filename
#' @param document_id
#'
#' @return
#' @export
PUT_bo_document <- function(conn, filepath, filename, document_id) {
  request <- check_bo_connection(conn)
  .body <- upload_file(filepath)
  request$headers[["Accept"]] <- "*/*"
  request$headers[["Accept-Encoding"]] <- "gzip, deflate"
  request$headers[["Content-Type"]] <- "multipart/form-data"
  url <- paste0(request$url, "/v1/documents/", document_id)
  response <- PUT(url, body = list(y = .body), request)
  report_api_error(request, response, paste(";PUT", filepath,filename,document_id), "PUT_bo_document", 94)
}

#' Title
#'
#' @param conn Connection reference
#' @param documentName
#' @param folder
#' @param newName
#'
#' @return
#' @export
copyBODocument <- function(conn, documentName, folder, newName) {
  request <- check_bo_connection(conn)
  doc <-
    get_bo_item_from_name(conn,
                      name = documentName,
                      folder = folder)
  if (nrow(doc)) {
    json <-
      list("document" = list("name" = newName)) %>% listToJSON()
    write(json, ".attachmentInfos")
    .body0 <-
      upload_file(".attachmentInfos", type = "application/json")
    url <-
      paste0(request$url, "/raylight/v1/documents?sourceId=", doc$SI_ID)
    response <-
      POST(
        url,
        body = .body0,
        encode = "json",
        request
      )
    report_api_error(
      request,
      response,
      paste("POST copy file", documentName, doc$SI_ID),
      "copyBODocument",
      271
    )
    content(response)
    mycat("Copy", documentName, "complete", level = "message")
    if (file.exists(".attachmentInfos")) {
      file.remove(".attachmentInfos")
    }
  } else {
    mycat("Copy document", documentName, "does not exist", level = "error")
  }
}

DELETE_bo_document <- function(conn, document_id) {
  request <- check_bo_connection(conn)
  url <- paste0(request$url, "/v1/documents/", document_id)
  response <- DELETE(url, request)
  report_api_error(request, response, paste(";DELETE",document_id), "delete_bo_document", 101)
}

GET_bo_document_control_selection <- function(conn, document, controlName) {
  document_id <- get_bo_item_id(document)
  inputcontrols <- GET_bo_raylight_endpoint(conn, documents = document_id, inputcontrols = "")
  inputcontrol <- inputcontrols %>% dplyr::filter(name == controlName)
  result <- GET_bo_raylight_endpoint(conn, documents = document_id, inputcontrols = inputcontrol$id, selection = "")
}

#' Title
#'
#' @param conn Connection reference
#' @param document Document as numeric id or tibble of properties
#' @param controlName
#'
#' @return
#' @export

get_bo_document_control_selection_set <- function(conn, document, controlName = NULL) {
  document_id <- get_bo_item_id(document)
  inputcontrols <- GET_bo_raylight_endpoint(conn, documents = document_id, inputcontrols = "")
  inputcontrol <- inputcontrols %>% dplyr::filter(name == controlName)
  inputcontrol <- GET_bo_raylight_endpoint(conn, documents = document_id, inputcontrols = inputcontrol$id)
  dataobjectId <- inputcontrol$assignedDataObject$`@refId`
  result <- GET_bo_raylight_endpoint(conn, documents = document_id, dataobjects = dataobjectId, lov = "")
  result$values
}

#' Title
#'
#' @param conn Connection reference
#' @param document Document as numeric id or tibble of properties
#' @param controlName
#' @param selections
#' @param all
#'
#' @return
#' @export
set_bo_document_control_selection <- function(conn, document, controlName, selections, all) {
  document_id <- get_bo_item_id(document)
  inputcontrols <- GET_bo_raylight_endpoint(conn,documents = document_id, inputcontrols = "")
  inputcontrol <- inputcontrols %>% dplyr::filter(name == controlName)
  v <- enframe(selections, name = NULL)
  l <- list(selection = list(value = list(selections)))
  b <- toJSON(l)
  result <- PUT_bo_raylight_endpoint(conn, documents = document_id, inputcontrols = inputcontrol$id, selection = "", body = l)
}

dateToBOQueryFilterDate <- function(d) {
  paste0(as.numeric(as.Date(d),origin="1970-01-01")*864,'00000')
}

queryFilterDateToDate <- function(d) {
  scaledDate = as.numeric(str_sub(d, end=-6))/864
  as.Date(as.POSIXct(scaledDate, origin="1970-01-01"))
}

#' Title
#'
#' @param conn Connection reference
#' @param document Document as numeric id or tibble of properties
#' @param dataprovider
#' @param startDate
#' @param endDate
#'
#' @return
#' @export
#'
set_bo_document_data_source_date_range <- function(conn, document, dataprovider, startDate, endDate) {
  request <- check_bo_connection(conn)
  document_id <- get_bo_item_id(document)
  dp <- GET_bo_raylight_endpoint(conn, documents=document_id, dataproviders='')
  dp %>% dplyr::filter(name == dataprovider)
  sdp <- GET_bo_raylight_endpoint(conn, documents=document_id, dataproviders=dp$id[1])
  request$headers[['Accept']]<-'text/xml'
  spc <- GET_bo_raylight_endpoint(request, documents=document_id, dataproviders=sdp$id, specification='')
  dates <- str_extract_all(spc, 'value="[0-9]{13}" type="Date"') %>% map(~str_extract(., pattern='[0-9]{13}')) %>% unlist()
  spc<-str_replace(spc, dates[1], dateToBOQueryFilterDate(startDate)) %>% str_replace(pattern = dates[2], replacement = dateToBOQueryFilterDate(endDate))
  request$headers[['Content-Type']]<-'text/xml'
  request$headers[['Accept']]<-'application/json'
  PUT_bo_raylight_endpoint(request, documents=document_id, dataproviders=dp$id[1], specification='', body = spc)
}

refreshBODataProvider <- function(conn, document_id, dataSourceId) {
  PUT_bo_raylight_endpoint(conn, documents = document_id, dataproviders = dataSourceId, parameters = '')
}

#' Title
#'
#' @param conn Connection reference
#' @param document Document as numeric id or tibble of properties
#' @param dataSourceType
#'
#' @return
#' @export
refresh_bo_document <- function(conn, document, dataSourceType = NULL) {
  document_id <- get_bo_item_id(document)
  dp <- GET_bo_raylight_endpoint(conn, documents = document_id, dataproviders = '')
  if (!isNullOrEmpty(dataSourceType)) dp %<>% dplyr::filter(dataSourceType == dataSourceType)
  dp$id %>% sapply(function(x) refreshBODataProvider(conn, document_id, x))
}

#' Title
#'
#' @param conn Connection Reference
#' @param document Document as numeric id or tibble of properties
#'
#' @return Document properties as tibble
#' @export
get_bo_document <- function(conn, document) {
  request <- check_bo_connection(conn)
  request$headers[["Accept-Encoding"]] <- "gzip, deflate"
  document_id <- get_bo_item_id(document)
  url <- paste0(request$url, "/v1/documents/", document_id)
  response <- GET(url, request)
  mycat("get_bo_document 249", ifelse(response$status_code == 200, paste("Get succeeded to", url), paste("Get failed", response$status_code, content(response))))
  document <- content(response)
  report_api_error(request, response, paste("GET document", document_id), "get_bo_document", 337)
  document
}

#' Title
#'
#' @param conn Connection reference
#' @param document Document as numeric id or tibble of properties
#' @param inputs Name of report inputs as path "reports/<report>/<element>"
#'
#' @return Inputs as tibble
#' @export
get_bo_report_inputs <- function(conn, document, inputs) {
  il <- str_split(inputs, '/') %>% unlist()
  if (il[1] == 'reports') {
    report <- il[2]
    element <- il[3]
    get_bo_report_element_data(conn, document, report, element)
  } else {
    mycat(level = 'error', "Unknown inputs format for",inputs)
  }
}



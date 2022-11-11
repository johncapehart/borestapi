#' @importFrom purrr quietly pluck
#' @importFrom tibble tibble enframe as_tibble is_tibble
#' @importFrom tidyr pivot_wider
#' @include api-utils.R
#'

testNumericArgument <- function(x) {
  quietly(function(x)!is.na(as.numeric(x)))(x)$result
}

#' Get data directly from a document data provider
#' See also get_bo_document_report
#'
#' @param conn Connection reference
#' @param document Document as numeric id or tibble of properties
#' @param provider_id Numeric id of the provider
#' @param ... Parameters for read_delim
#'
#' @return Tibble of data
#' @export
get_bo_document_data <- function(conn, document, provider_id, ...) {
  request <- check_bo_connection(conn)
  document_id <- get_bo_item_id(document)
  request$headers[["Accept"]] <- "text/plain"
  url <- paste(url, provider_id, "flows/0", sep = "/")
  response <- GET(url = url, request)
  df <- content(response, as = "text", encoding = "UTF-8") %>% read_delim(delim = ";", show_col_types = FALSE, ...)
  report_request_result(request, response, paste("Get document data rows", nrow(df), "columns", ncol(df)), "get_bo_document_data", 246)
  df
}

#' Close BO Webi document
#'
#' @param conn Connection reference
#' @param document Document as numeric id or tibble of properties
#' @param save If true save the document before closing
#'
#' @return Response content
#' @export
close_bo_document <- function(conn, document, save = FALSE) {
  request <- check_bo_connection(conn)
  document_id <- get_bo_item_id(document)
  url <- paste(request$url, "raylight/v1/documents", document_id, "occurrences", sep = "/")
  response <- GET(url, request)
  report_request_result(request, response, paste(";GET occurances", document_id), "close_bo_document", 55)
  occurrance <- content(response)$occurrences$occurrence
  if (length(occurrance) == 0) {
    log_message("No occurences to close for", document_id)
    return()
  } else if (length(occurrance) > 1) {
    log_message(length(occurrance), "occurences to found for", document_id)
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
    report_request_result(request, response, paste("Closing document", document$SI_Name, ";PUT occurance", occurrance[['state']]), "close_bo_document", 71)
    return_bo_response_content(response)
  }
}

#' Upload a new webi document
#'
#' @param conn Connection reference
#' @param filepath Path to document (including filename)
#' @param filename name of document
#' @param parent_folder Numeric id of parent folder for document
#'
#' @return Response content
#' @export
#' @noRd
post_bo_document <- function(conn, filepath, filename, parent_folder) {
  request <- check_bo_connection(conn)
  .body <- upload_file(filepath)
  request$headers[["Accept"]] <- "*/*"
  request$headers[["Accept-Encoding"]] <- "gzip, deflate"
  request$headers[["Content-Type"]] <- "multipart/form-data"
  url <- paste0(request$url, "/infostore/folder/", parent_folder, "/file")
  response <- POST(url, body = list(y = .body), request)
  report_request_result(request, response, paste(";POST", filepath,filename), "post_bo_document", 83)
}

#' Upload an existing BO Webi document
#'
#' @param conn Connection reference
#' @param filepath Path to document (including filename)
#' @param filename name of document
#' @param document_id Numeric id of Webi document
#'
#' @return Response content
#' @export
#' @noRd
put_bo_document <- function(conn, filepath, filename, document_id) {
  request <- check_bo_connection(conn)
  .body <- upload_file(filepath)
  request$headers[["Accept"]] <- "*/*"
  request$headers[["Accept-Encoding"]] <- "gzip, deflate"
  request$headers[["Content-Type"]] <- "multipart/form-data"
  url <- paste0(request$url, "/v1/documents/", document_id)
  response <- PUT(url, body = list(y = .body), request)
  report_request_result(request, response, paste(";PUT", filepath,filename,document_id), "put_bo_document", 94)
}

#' Copy a Webi document
#'
#' @param conn Connection reference
#' @param document Numeric id or tibble of properties of Webi document
#' @param parent_folder Numeric id of parent folder for copy
#' @param destination_document_name Name of destination document
#'
#' @return Response content
#' @export
#' @noRd
copyBODocument <- function(conn, document, parent_folder, destination_document_name) {
  request <- check_bo_connection(conn)
  document_id <- get_bo_item_id(document)
    json <-
      list("document" = list("name" = destination_document_name)) %>% listToJSON()
    write(json, ".attachmentInfos")
    .body0 <-
      upload_file(".attachmentInfos", type = "application/json")
    url <-
      paste0(request$url, "/raylight/v1/documents?sourceId=", document_id)
    response <-
      POST(
        url,
        body = .body0,
        encode = "json",
        request
      )
    report_request_result(
      request,
      response,
      paste("POST copy file", document_id, "copyBODocument line 139")
    )
    content(response)
    log_message("Copy", document_id, "to", destination_document_name, "complete", level = "message")
    if (file.exists(".attachmentInfos")) {
      file.remove(".attachmentInfos")
    }
}

delete_bo_document <- function(conn, document_id) {
  request <- check_bo_connection(conn)
  url <- paste0(request$url, "/v1/documents/", document_id)
  response <- DELETE(url, request)
  report_request_result(request, response, paste(";DELETE",document_id), "delete_bo_document", 101)
}

get_bo_document_controls <- function(conn, document) {
  document_id <- get_bo_item_id(document)
  get_bo_raylight_endpoint(conn, documents = document_id, inputcontrols = "")
}

get_bo_document_control_selection <- function(conn, document, control_name) {
  document_id <- get_bo_item_id(document)
  inputcontrols <-get_bo_document_controls(conn, document)
  inputcontrol <- inputcontrols %>% dplyr::filter(name == control_name)
  result <- get_bo_raylight_endpoint(conn, documents = document_id, inputcontrols = inputcontrol$id, selection = "")
}

#' Title
#'
#' @param conn Connection reference
#' @param document Document as numeric id or tibble of properties
#' @param control_name Name of the control
#'
#' @return Control selection set as tibble
#' @export
#' @noRd
get_bo_document_control_selection_set <- function(conn, document, control_name = NULL) {
  document_id <- get_bo_item_id(document)
  inputcontrols <- get_bo_document_controls(conn, document)
  if (!is_null_or_empty(control_name)) {
      inputcontrol <- inputcontrols %>% dplyr::filter(name == control_name)
  }
  inputcontrol <- get_bo_raylight_endpoint(conn, documents = document_id, inputcontrols = inputcontrol$id)
  dataobjectId <- inputcontrol$assignedDataObject$`@refId`
  result <- get_bo_raylight_endpoint(conn, documents = document_id, dataobjects = dataobjectId, lov = "")
  result$values
}

#' Title
#'
#' @param conn Connection reference
#' @param document Document as numeric id or tibble of properties
#' @param control_name Name of the control
#' @param selections Selected items
#' @param all Select all items
#'
#' @return Response content
#' @export
#' @noRd
set_bo_document_control_selection <- function(conn, document, control_name, selections, all) {
  document_id <- get_bo_item_id(document)
  inputcontrols <- get_bo_raylight_endpoint(conn,documents = document_id, inputcontrols = "")
  inputcontrol <- inputcontrols %>% dplyr::filter(name == control_name)
  v <- enframe(selections, name = NULL)
  l <- list(selection = list(value = list(selections)))
  b <- toJSON(l)
  put_bo_raylight_endpoint(conn, documents = document_id, inputcontrols = inputcontrol$id, selection = "", body = l)
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
#' @param dataprovider Name of data provider from document
#' @param startDate Start of date range
#' @param endDate End of date range
#'
#' @return Response content
#' @export
#' @noRd
set_bo_document_data_source_date_range <- function(conn, document, dataprovider, startDate, endDate) {
  request <- check_bo_connection(conn)
  document_id <- get_bo_item_id(document)
  dp <- get_bo_raylight_endpoint(conn, documents=document_id, dataproviders='')
  dp %>% dplyr::filter(name == dataprovider)
  sdp <- get_bo_raylight_endpoint(conn, documents=document_id, dataproviders=dp$id[1])
  request$headers[['Accept']]<-'text/xml'
  spc <- get_bo_raylight_endpoint(request, documents=document_id, dataproviders=sdp$id, specification='')
  dates <- str_extract_all(spc, 'value="[0-9]{13}" type="Date"') %>% map(~str_extract(., pattern='[0-9]{13}')) %>% unlist()
  spc<-str_replace(spc, dates[1], dateToBOQueryFilterDate(startDate)) %>% str_replace(pattern = dates[2], replacement = dateToBOQueryFilterDate(endDate))
  request$headers[['Content-Type']]<-'text/xml'
  request$headers[['Accept']]<-'application/json'
  put_bo_raylight_endpoint(request, documents=document_id, dataproviders=dp$id[1], specification='', body = spc)
}

refreshBODataProvider <- function(conn, document_id, dataSourceId) {
  put_bo_raylight_endpoint(conn, documents = document_id, dataproviders = dataSourceId, parameters = '')
}

#' Title
#'
#' @param conn Connection reference
#' @param document Document as numeric id or tibble of properties
#' @param dataSourceType Type of data source to refresh
#'
#' @return Response content
#' @export
refresh_bo_document <- function(conn, document, dataSourceType = NULL) {
  document_id <- get_bo_item_id(document)
  dp <- get_bo_raylight_endpoint(conn, documents = document_id, dataproviders = '')
  if (!is_null_or_empty(dataSourceType)) dp %<>% dplyr::filter(dataSourceType == dataSourceType)
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
  log_message("get_bo_document 249", ifelse(response$status_code == 200, paste("Get succeeded to", url), paste("Get failed", response$status_code, content(response))))
  document <- content(response)
  report_request_result(request, response, paste("GET document", document_id), "get_bo_document", 337)
  document
}

#' Get inputs from a report table
#'
#' @param conn Connection reference
#' @param document Document as numeric id or tibble of properties
#' @param inputs Name of report inputs as path "reports/report/element"
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
    log_message(level = 'error', "Unknown inputs format for",inputs)
  }
}



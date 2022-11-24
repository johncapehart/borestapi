# Controls break ----------------------------------------------------------------

#' Get input controls for document or report
#'
#' @param conn Connection reference
#' @param document Document as numeric id or tibble of properties
#' @param report Report name (omit for document level controls)
#' @param control Control name (omit to return all controls)
#' @param all_info Return all control details
#'
#' @return Controls as tibble
#' @export
get_bo_control_details <- function(conn, document, report = NULL, control = '', all_info = FALSE) {
  document_id <- get_bo_item_id(document)
  if (all_info) {
    query = list('allInfo'='true')
  } else {
    query = NULL
  }
  if (is_null_or_empty(report)) {
    path = list(documents = document_id, inputcontrols = '')
    result <- request_bo_raylight_endpoint(conn, documents = document_id, inputcontrols = '', query = query)
  } else {
    report0 <- get_bo_report_details(conn, document, report)
    result <- request_bo_raylight_endpoint(conn, documents = document_id, reports = report0$id, inputcontrols = '', query = query)
  }
  if (!is_null_or_empty(control)) {
    result <- result %>% dplyr::filter(name == control | id == control)
  }
  result
}

#' Get the selection state of a document control
#'
#' @param conn Connection reference
#' @param document Document as numeric id or tibble of properties
#' @param control_name Name of control
#'
#' @return '@all' or tibble of selected levels
#' @export
get_bo_control_selection <- function(conn, document, report = '', control) {
  document_id <- get_bo_item_id(document)
  inputcontrols <- get_bo_control_details(conn, document, report, control)
  if (is_null_or_empty(report)) {
    selection <- request_bo_raylight_endpoint(conn, documents = document_id, inputcontrols = inputcontrols$id, selection = "")
  } else {
    report0 <- get_bo_report_details(conn, document, report)
    selection <- request_bo_raylight_endpoint(conn, documents = document_id, reports = report0$id, inputcontrols = inputcontrols$id, selection = "")
  }
  selection
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
get_bo_control_selection_set <- function(conn, document, report = '', control = '') {
  document_id <- get_bo_item_id(document)
  inputcontrols <- get_bo_control_details(conn, document, report, control)
  if (is_null_or_empty(report)) {
    inputcontrol <- request_bo_raylight_endpoint(conn, documents = document_id, inputcontrols = inputcontrols$id)
    dataobjectId <- inputcontrol$assignedDataObject$`@refId`
  } else {
    report0 <- get_bo_report_details(conn, document, report)
    inputcontrol <- request_bo_raylight_endpoint(conn, documents = document_id, reports = report0$id, inputcontrols = inputcontrols$id)
    dataobjectId <- inputcontrol$assignedDataObject$`@refId`
  }
  result <- request_bo_raylight_endpoint(conn, documents = document_id, dataobjects = dataobjectId, lov = "")
  result$values$value
}

#' Set the selected items for a control
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
set_bo_control_selection <- function(conn, document, report = NULL, control, selections = NULL, all = FALSE) {
  document_id <- get_bo_item_id(document)
  inputcontrol <-get_bo_control_details(conn, document, report, control)
  if (all) {
    body = list('selection'=list('@all'=TRUE))
  } else {
    body <- list('selection'=list(value=selections))
  }
  if (is_null_or_empty(report)) {
    request_bo_raylight_endpoint(conn, documents = document_id, inputcontrols = inputcontrol$id, selection='', body = body)
  } else {
    report0 <- get_bo_report_details(conn, document, report)
    request_bo_raylight_endpoint(conn, documents = document_id, reports = report0$id, inputcontrols = inputcontrol$id, selection='', body = body)
  }
}

dateToBOQueryFilterDate <- function(d) {
  paste0(as.numeric(as.Date(d),origin="1970-01-01")*864,'00000')
}

queryFilterDateToDate <- function(d) {
  scaledDate = as.numeric(str_sub(d, end=-6))/864
  as.Date(as.POSIXct(scaledDate, origin="1970-01-01"))
}

#' @title Set the date range for a control
#' @details Not tested
#' @param conn Connection reference
#' @param document Document as numeric id or tibble of properties
#' @param dataprovider Name of data provider from document
#' @param startDate Start of date range
#' @param endDate End of date range
#'
#' @return Response content
#' @export
#' @noRd
set_bo_data_source_date_range <- function(conn, document, dataprovider, startDate, endDate) {
  request <- check_bo_connection(conn)
  document_id <- get_bo_item_id(document)
  dp <- request_bo_raylight_endpoint(conn, documents=document_id, dataproviders='')
  dp %>% dplyr::filter(name == dataprovider)
  sdp <- request_bo_raylight_endpoint(conn, documents=document_id, dataproviders=dp$id[1])
  request %<>% httr2::req_headers('Accept' = 'text/xml')
  spc <- request_bo_raylight_endpoint(request, documents=document_id, dataproviders=sdp$id, specification='')
  dates <- str_extract_all(spc, 'value="[0-9]{13}" type="Date"') %>% map(~str_extract(., pattern='[0-9]{13}')) %>% unlist()
  spc<-str_replace(spc, dates[1], dateToBOQueryFilterDate(startDate)) %>% str_replace(pattern = dates[2], replacement = dateToBOQueryFilterDate(endDate))
  request %<>% httr2::req_headers('Content-Type'= 'text/xml',
    'Accept' = 'application/json'
  )
  request_bo_raylight_endpoint(request, documents=document_id, dataproviders=dp$id[1], specification='', body = spc)
}

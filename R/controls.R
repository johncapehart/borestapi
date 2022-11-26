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
    result <- result %>% dplyr::filter(.data$name == control | .data$id == control)
  }
  result
}

#' Get the selection state of a document control
#'
#' @param conn Connection reference
#' @param document Document as numeric id or tibble of properties
#' @param report Identifier of report for report level controls, blank otherwise
#' @param control Identifier of control
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
#' @param report Identifier of report for report level controls, blank otherwise
#' @param control Identifier of control
#'
#' @return Control selection set as tibble
#' @export
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
#' @param report Identifier of report for report level controls, blank otherwise
<<<<<<< HEAD
#' @param control Identifier of the control
=======
#' @param control_name Name of the control
>>>>>>> 0e901f710c7a4e4a0788f71e359e608a2136b1c8
#' @param selections Selected items
#' @param all Select all items
#'
#' @return Response content
#' @export
set_bo_control_selection <- function(conn, document, report = NULL, control, selections = NULL, all = FALSE) {
  document_id <- get_bo_item_id(document)
  inputcontrol <-get_bo_control_details(conn, document, report, control)
  if (all) {
    body = list('selection'=list('@all'=TRUE))
  } else {
    body <- list('selection'=list(value=selections))
  }
  if (is_null_or_empty(report)) {
    request_bo_raylight_endpoint(conn, documents = document_id, inputcontrols = inputcontrol$id, selection='', body = body, method = 'PUT')
  } else {
    report0 <- get_bo_report_details(conn, document, report)
    request_bo_raylight_endpoint(conn, documents = document_id, reports = report0$id, inputcontrols = inputcontrol$id, selection='', body = body, method = 'PUT')
  }
}

dateToBOQueryFilterDate <- function(d) {
  paste0(as.numeric(as.Date(d),origin="1970-01-01")*864,'00000')
}

queryFilterDateToDate <- function(d) {
  scaledDate = as.numeric(stringr::str_sub(d, end=-6))/864
  as.Date(as.POSIXct(scaledDate, origin="1970-01-01"))
}

#' @title Get the date range for a data provider specification
#' @param conn Connection reference
#' @param document Document as numeric id or tibble of properties
#' @param dataprovider Name of data provider from document
#'
#' @return XML string of data provider specification
#' @export
get_bo_data_provider_specification <- function(conn, document, dataprovider) {
  request <- check_bo_connection(conn)
  document_id <- get_bo_item_id(document)
  dp <- get_bo_data_provider_details(conn, document, data_provider = dataprovider)
  request_bo_raylight_endpoint(conn, documents=document_id, dataproviders=dp$id, specification='', accept = 'text/xml')
}

#' @title Set the date range provider specification
#'
#' @param conn Connection reference
#' @param document Document as numeric id or tibble of properties
#' @param dataprovider Name of data provider from document
#' @param specification XML specification of provider
#'
#' @return NULL
#' @export
set_bo_data_provider_specification <- function(conn, document, dataprovider, specification) {
  request <- check_bo_connection(conn)
  document_id <- get_bo_item_id(document)
  dp <- get_bo_data_provider_details(conn, document, data_provider = dataprovider)
  request_bo_raylight_endpoint(conn, documents=document_id, dataproviders=dataprovider, specification='',
                               accept = 'application/json', body = specification, content_type = 'text/xml', method = 'PUT')
}

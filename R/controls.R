#' @importFrom magrittr %<>% %>%
#' @include api-utils.R
#'

#----------------------------------------------------------------
# document controls

get_bo_control_details <- function(conn, document, control_name = '') {
  document_id <- get_bo_item_id(document)
  result <- get_bo_raylight_endpoint(conn, documents = document_id, inputcontrols = control_name)
  result %>% bind_list()
}

#' Get the selection state of a document control
#'
#' @param conn Connection reference
#' @param document Document as numeric id or tibble of properties
#' @param control_name Name of control
#'
#' @return '@all' or tibble of selected levels
#' @export
get_bo_control_selection <- function(conn, document, control_name) {
  document_id <- get_bo_item_id(document)
  inputcontrols <-get_bo_control_details(conn, document)
  inputcontrol <- inputcontrols %>% dplyr::filter(name == control_name)
  result <- get_bo_raylight_endpoint(conn, documents = document_id, inputcontrols = inputcontrol$id, selection = "")
  if (result[['@all']]) {
    return('@all')
  } else {
    return(result$value %>% bind_list())
  }
}

set_bo_control_selection <- function(conn, document, control_name, selection) {
  document_id <- get_bo_item_id(document)
  inputcontrols <-get_bo_control_details(conn, document)
  inputcontrol <- inputcontrols %>% dplyr::filter(name == control_name)
  put_bo_raylight_endpoint(conn, documents = document_id, inputcontrols = inputcontrol$id, selection = "")
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
get_bo_control_selection_set <- function(conn, document, control_name = NULL) {
  document_id <- get_bo_item_id(document)
  inputcontrols <- get_bo_control_details(conn, document)
  inputcontrols <- inputcontrols %>% dplyr::filter(name == control_name)
  inputcontrol <- get_bo_raylight_endpoint(conn, documents = document_id, inputcontrols = inputcontrols$id)
  dataobjectId <- inputcontrol$assignedDataObject$`@refId`
  result <- get_bo_raylight_endpoint(conn, documents = document_id, dataobjects = dataobjectId, lov = "")
  values <- result$values %>% flatten_scalars()
  values
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
set_bo_control_selection <- function(conn, document, control_name, selections, all) {
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
set_bo_data_source_date_range <- function(conn, document, dataprovider, startDate, endDate) {
  request <- check_bo_connection(conn)
  document_id <- get_bo_item_id(document)
  dp <- get_bo_raylight_endpoint(conn, documents=document_id, dataproviders='')
  dp %>% dplyr::filter(name == dataprovider)
  sdp <- get_bo_raylight_endpoint(conn, documents=document_id, dataproviders=dp$id[1])
  request %<>% httr2::req_headers('Accept' = 'text/xml')
  spc <- get_bo_raylight_endpoint(request, documents=document_id, dataproviders=sdp$id, specification='')
  dates <- str_extract_all(spc, 'value="[0-9]{13}" type="Date"') %>% map(~str_extract(., pattern='[0-9]{13}')) %>% unlist()
  spc<-str_replace(spc, dates[1], dateToBOQueryFilterDate(startDate)) %>% str_replace(pattern = dates[2], replacement = dateToBOQueryFilterDate(endDate))
  request %<>% httr2::req_headers('Content-Type'= 'text/xml',
    'Accept' = 'application/json'
  )
  put_bo_raylight_endpoint(request, documents=document_id, dataproviders=dp$id[1], specification='', body = spc)
}

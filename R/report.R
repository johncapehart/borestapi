get_bo_report_name <- function(report) {
  # report may have a 'reports' prefix
  il <- str_split(report, '/') %>% unlist()
  if (length(il) == 1) {
    # no prefix
    report = il
  } else {
    if (il[1] == 'reports') {
      # has prefix
      report = il[2]
    }
  }
}

#' Title
#'
#' @param conn Connection reference
#' @param document Document as numeric id or tibble of properties
#' @param report Name of report tabl in document
#' @param skip Rows of data to skip before header
#'
#' @return Dataframe of report data
#' @export

get_bo_document_report <- function(conn, document, report, skip=0) {
  mycat("Getting report data", report, "for document", document, ";get_bo_document_report 220")
  document_id = get_bo_item_id(document)
  report_name <- get_bo_report_name(report)
  reports <- GET_bo_raylight_endpoint(conn, documents = document_id, reports = '')
  report <- reports %>%
    dplyr::filter(id == report | name == report_name) %>%
    tail(1)
  reportId <- report$id
  request <- check_bo_connection(conn)
  request$headers[["Accept"]] <- "text/csv"
  result <- GET_bo_raylight_endpoint(request, documents = document_id, reports = reportId)
  dataset <- result %>% read_delim(delim = ";",skip=skip)
  mycat("Report data retrieved", report_name, "for document", document, 'with', nrow(dataset), 'rows', ";get_bo_document_report 220")
  return(dataset)
}

#' Title
#'
#' @param conn Connection reference
#' @param document Document as numeric id or tibble of properties
#' @param report Name of report tab in document
#' @param element Name of element in report
#'
#' @return tibble with data from report element
#' @export
#'
get_bo_report_element_data <- function(conn, document, report, element) {
  mycat("Getting report", report, "for document", document, ";get_bo_document_report 33")
  document_id = get_bo_item_id(document)
  reports <- GET_bo_raylight_endpoint(conn, documents = document_id, reports = '')
  report2 <- reports %>%
    dplyr::filter(id == report | name == report) %>%
    tail(1)
  reportId <- report2$id
  elements <- GET_bo_raylight_endpoint(conn = conn, documents = document_id, reports = reportId, elements = '')
  elements %<>% dplyr::filter(id == element | name == element)
  elementId <- elements$id
  dataset <- GET_bo_raylight_endpoint(conn, documents = document_id, reports = reportId, elements = elementId, dataset = '')
  names <- dataset$metadata$value[['$']]
  rows <- dataset$row$value %>% unlist()
  result <- tibble(value=rows, name=names) %>% pivot_wider()
  # result$inputs.document <- document # not wrapping document in list produces infinite recursion (!)
  result$inputs.document <- list(document)
  result$inputs.document_id <- document_id
  result$inputs.reportId <- reportId
  result$inputs.elementId <- elementId
  mycat(level = 'info', "Report data retrieved", report2, "for document", document)
  result
}


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

#' Get data from a BO document report tab
#' @description Use get_bo_report_data to get data from a tab in a BO document. Be sure to save the report
#' after refreshing data in BO
#' @details
#' Depending on the layout of the report tab you may need to skip rows in the output to get to the data
#' The data returned is the table as it was last saved in BO
#' @param conn Connection reference obtained from open_bo_connection
#' @param document Document as numeric id or tibble of properties obtained from get_bo_item_from_name
#' @param report Name of report tab in document
#' @param skip Rows of data to skip before header
#'
#' @return Tibble of report data
#' @export

get_bo_report_data <- function(conn, document, report, skip=0) {
  logger::log_info("Getting report data", "{report}", "for document", "{document}", ";get_bo_report_data 220")
  document_id = get_bo_item_id(document)
  report_name <- get_bo_report_name(report)
  reports <- get_bo_raylight_endpoint(conn, documents = document_id, reports = '') %>% bind_list()
  report <- reports %>%
    dplyr::filter(id == report | name == report_name) %>%
    tail(1)
  reportId <- report$id
  result <- get_bo_raylight_endpoint(conn, documents = document_id, reports = reportId, accept='text/csv')
  dataset <- result %>% read_delim(delim = ";",show_col_types = FALSE, skip = skip)
  logger::log_info(paste(nrow(dataset), "rows retrieved from report", report_name, "in document", "{document}", ";get_bo_report_data 220"))
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
#' @noRd
get_bo_report_element_data <- function(conn, document, report, element) {
  logger::log_info("Getting report","{report}", "for document", "{document}", ";tget_bo_report_data 33")
  document_id = get_bo_item_id(document)
  reports <- get_bo_raylight_endpoint(conn, documents = document_id, reports = '')
  reports <- bind_list(reports)
  report2 <- reports %>%
    dplyr::filter(id == report | name == report) %>%
    tail(1)
  reportId <- report2$id
  elements <- get_bo_raylight_endpoint(conn = conn, documents = document_id, reports = reportId, elements = '') %>% bind_list()
  elements %<>% dplyr::filter(id == element | name == element)
  elementId <- elements$id
  data <- get_bo_raylight_endpoint(conn, documents = document_id, reports = reportId, elements = elementId, dataset = '')
  metadata <- data$metadata %>% flatten_scalars() %>% bind_list()
  rows <- data$row %>% unlist()
  names <- metadata[['$']]
  result <- tibble(value=rows, name=names) %>% pivot_wider()
  result$document <- list(document)
  result$document_id <- document_id
  result$reportId <- reportId
  result$elementId <- elementId
  logger::log_info(paste("Report data retrieved", "{report2$name}", "for document", "{document$SI_NAME}"))
  result
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
    logger::log_error("Unknown inputs format for",inputs)
  }
}




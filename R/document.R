#' @importFrom magrittr %<>% %>%
#' @importFrom tibble tibble enframe as_tibble is_tibble
#' @importFrom tidyr pivot_wider
#' @include api-utils.R
#'

testNumericArgument <- function(x) {
  quietly(function(x)!is.na(as.numeric(x)))(x)$result
}

#' Get data directly from a document data provider
#' See also get_bo_report_data
#'
#' @param conn Connection reference
#' @param document Document as numeric id or tibble of properties
#' @param provider_id Id of the provider
#' @param ... Parameters for read_delim
#'
#' @return Tibble of data
#' @export
get_bo_data_provider_data <- function(conn, document, data_provider, ...) {
  request <- check_bo_connection(conn)
  document_id <- get_bo_item_id(document)
  request %<>% httr2::req_headers("Accept" = "text/plain")
  request %<>% httr2::req_url_path_append("raylight/v1/documents", document_id)
  request %<>% httr2::req_url_path_append("dataproviders", data_provider, 'flows', "0")
  response <- httr2::req_perform(request)
  df <- httr2::resp_body_string(response) %>% read_delim(delim = ";", show_col_types = FALSE, ...)
  report_request_result(request, response, paste("Get document data rows", nrow(df), "columns", ncol(df)), "get_bo_data_provider_data", 246)
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
  request$url <- paste(request$url, "raylight/v1/documents", document_id, "occurrences", sep = "/")
  response <- httr2::req_perform(request)
  report_request_result(request, response, paste(";GET occurances", document_id), "close_bo_document", 55)
  occurrance <- httr2::resp_body_json(response)$occurrences$occurrence
  if (length(occurrance) == 0) {
    logger::log_info("No occurences to close for", document_id)
    return()
  } else if (length(occurrance) > 1) {
    logger::log_info(length(occurrance), "occurences to found for", document_id)
  }
  occurrance %<>% keep(function(x)x$state!='Unused') %>% head(1)
  if (length(occurrance) == 1) {
    occurrance %<>% unlist()
    request$url <- paste(request$url, occurrance[["id"]], sep = "/")
    if (save) {
    } else {
      body <- list("occurrence" = list(state = 'Unused')) %>% listToJSON()
      request %<>% httr2::req_body_json(body)
    }
    request %<>% httr2::req_method('PUT')
    response <- httr2::req_perform(request)
    report_request_result(request, response, paste("Closing document", document$SI_NAME, ";PUT occurance", occurrance[['state']]), "close_bo_document", 71)
    httr2::resp_body_json(response)
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
post_bo_document <- function(conn, filename, parent_folder, filepath = filename) {
  request <- check_bo_connection(conn)
  request %<>% httr2::req_body_file(path=filepath) %>% httr2::req_headers("Accept" = "*/*",
    "Accept-Encoding" = "gzip, deflate",
    "Content-Type" = "multipart/form-data"
  ) %>% httr2::req_url_path_append("infostore/folder", parent_folder, "/file")
  response <- httr2::req_perform(request)
  report_request_result(request, response, paste(";POST", filepath,filename), "post_bo_document", 83)
  httr2::resp_body_json(request)
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
  request %<>% httr2::req_url_path_append("infostore/folder", parent_folder, "file") %>%
    httr2::req_body_file(path=filepath) %>%
    httr2::req_headers("Accept" = "*/*",
      "Accept-Encoding" = "gzip, deflate",
      "Content-Type" = "multipart/form-data"
    ) %>%
    httr2::req_method('PUT')
  response <- httr2::req_perform(request)
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
copy_bo_document <- function(conn, document, parent_folder, destination_document_name) {
  request <- check_bo_connection(conn)
  document_id <- get_bo_item_id(document)
  body <- list("document" = list("name" = destination_document_name))
  request %<>% httr2::req_body_json(body)
  request %<>% httr2::req_url_path_append('raylight/v1/documents') %>%
    httr2::req_url_query("sourceId" = document_id)
  response <- httr2::req_perform(request)
  report_request_result(
    request,
    response,
    paste("POST copy file", document_id, "copyBODocument line 139")
  )
  logger::log_info(paste("Copy", document_id, "to", destination_document_name, "complete"))
  httr2::resp_body_json(response)
}

delete_bo_document <- function(conn, document) {
  request <- check_bo_connection(conn)
  document_id <- get_bo_item_id(document)
  request %<>% httr2::req_url_path_append("raylight/v1/documents", document_id) %>%
    httr2::req_method('DELETE')
  response <- httr2::req_perform(request)
  report_request_result(request, response, paste(";DELETE",document_id, ";t delete_bo_document line 146"))
  httr2::resp_body_json(response)
}

#' Get the meta data for a Webi document
#'
#' @param conn Connection Reference
#' @param document Document as numeric id or tibble of properties
#'
#' @return Document properties as tibble
#' @export
get_bo_document_details <- function(conn, document) {
  request <- check_bo_connection(conn)
  request %<>% httr2::req_headers("Accept-Encoding" = "gzip, deflate")
  document_id <- get_bo_item_id(document)
  request %<>% httr2::req_url_path_append("/v1/documents", document_id)
  response <- httr2::req_perform(request)
  report_request_result(request, response, paste(";d GET", document_id, ";t get_bo_document_details line 163"))
  httr2::resp_body_json(response) %>% as_tibble()
}

refresh_bo_data_provider <- function(conn, document_id, provider_id) {
  put_bo_raylight_endpoint(conn, documents = document_id, dataproviders = provider_id, parameters = '')
  logger::log_info(paste("Refreshed data for", document_id, provider_id, ";trefresh_bo_data_provider line 170"))
}

get_bo_data_provider_details <- function(conn, document, data_provider = '') {
  document_id <- get_bo_item_id(document)
  dp <- get_bo_raylight_endpoint(conn, documents = document_id, dataproviders = data_provider) %>% bind_list()
  return(dp)
}

#' Title
#'
#' @param conn Connection reference
#' @param document Document as numeric id or tibble of properties
#' @param dataSourceType Type of data source to refresh
#'
#' @return Response content
#' @export
refresh_bo_document_data_provider <- function(conn, document, data_provider = NULL) {
  document_id <- get_bo_item_id(document)
  dp <- get_bo_raylight_endpoint(conn, documents = document_id, dataproviders = '') %>% bind_list()
  if (!is_null_or_empty(data_provider)) {
    dp %<>% dplyr::filter(id == data_provider)
  }
  dp$id %>% sapply(function(x) refresh_bo_data_provider(conn, document_id, x))
}

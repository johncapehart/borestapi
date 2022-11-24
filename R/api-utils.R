
#' Report error in API call
#'
#' @param request httr::request
#' @param response httr::request
#' @param ... list of text arguments for message
#'
#' @noRd
report_request_result <- function(request, response, ...) {
  inputs <- list(...)
  message1 <- paste(head(inputs, 1), collapse = " ")
  message2 <- paste(tail(inputs, -1), collapse = " ")
  # if(is.na(message1)||message1=='NA') #browser()()
  if (response$status_code == 200) {
    logger::log_info(paste(message1, "succeeded", ";d", message2, "{response$url}"))
  } else {
    logger::log_error(paste(message1, "failed", response$status_code, ";d", message2,  "{response$url}", request$verb, "{content(response, as='text', encoding='UTF-8')}"))
    # throw()
  }
}

#' Return Id from item properties or from numeric value
#'
#' @param item Item properties as tibble or item id as numeric value
#'
#' @return Numeric id of item

get_bo_item_id <- function(item) {
  if (quiet_is_numeric(item)) {
    return(item)
  }
  if (is_list(item) || is_tibble(item) || is.environment(item)) {
    if ('SI_ID' %in% names(item)) {
      # for tibble returned by querys
      return(item$SI_ID)
    }
    if ('id' %in% names(item)) {
      # for tibble returned by raylight
      return(item$id)
    }
  }
  return(NULL)
}

append_query_conditions <- function(query, name = NULL, parent_folder = NULL, kind = NULL, owner = NULL, id=NULL) {
  addAnd <- function(needAnd) {
    if (needAnd) {
      return(' AND')
    }
    return('')
  }

  needAnd = FALSE
  if (hasArg("name") && !is.null(name) && str_length(name) > 0) {
    if (grepl('%', name, fixed = TRUE)) {
      query <- paste0(query, " SI_NAME LIKE '", name, "'")
    } else {
      query <- paste0(query, " SI_NAME='", name, "'")
    }
    needAnd = TRUE
  }
  if (hasArg("parent_folder") && !is.null(parent_folder) && parent_folder > 0) {
    query <-
      paste0(query, addAnd(needAnd), " SI_PARENT_FOLDER=", parent_folder)
    needAnd = TRUE
  }
  if (hasArg("kind") && !is.null(kind) && str_length(kind) > 0) {
    query <- paste0(query, addAnd(needAnd), " SI_KIND='", kind, "'")
    needAnd = TRUE
  }
  if (hasArg("owner") && !is.null(owner) && str_length(owner) > 0) {
    query <- paste0(query, addAnd(needAnd), " SI_OWNER='", owner, "'")
    needAnd = TRUE
  }
  query
}

bind_bo_query_results_to_tibble <- function(entries) {
  items <- entries %>%
    lapply(function(x) {
      as_tibble(x)
    }) %>%
    bind_rows()
  return(items)
}

#' Retrieve BO item proerties
#'
#' @description Get the properties of items as tibble.
#' @details
#' After opening a connection with open_bo_connection you can retrieve the properties of a items with this function.
#' The properties returned contain the item id in the SI_ID column. This matches the ID in the properties page in the BO web GUI.
#' If you have the id of the parent folder that will narrow the search in case there are duplicate item names. If more
#' than one item is returned by the query the tibble will contain multiple rows
#' @param conn Connection reference
#' @param name Name of the item (optional). You can use SQL wilcard % in the name.
#' @param parent_folder Numeric id of the parent folder (optional)
#' @param kind Kind of item (optional)
#' @param owner Owner of item (optional)
#' @param id Id of item (optional)
#'
#' @return Tibble of item properties
#' @examples
#' # get the id of an item. Normally you would call open_bo_connection()
#' conn= open_bo_connection(); get_bo_item(conn, name='test',parent_folder=3944223)$SI_ID
#' @examples
#' # get all the Excel documents in a folder
#' get_bo_item(open_bo_connection(),parent_folder = 3944223, kind = 'Excel')
#' @export
get_bo_item <- function(conn, name = NULL, parent_folder = NULL, kind = NULL, owner = NULL, id = NULL) {
  request <- check_bo_connection(conn)
  query <- paste0(
    "SELECT SI_KIND, SI_ID, SI_PARENT_FOLDER, SI_NAME, SI_OWNER, SI_CREATION_TIME, SI_UPDATE_TS, SI_PATH FROM CI_INFOOBJECTS WHERE"
  )
  query <- append_query_conditions(query, name, parent_folder, kind, owner, id)
  #query <- paste(query, "ORDER BY SI_NAME ASC SI_CREATION_TIME DESC") # ORDER BY not supported
  body <- list("query" = query)
  request %<>% httr2::req_body_json(body)
  request$url <- paste0(request$url, "/v1/cmsquery?page=1&pagesize=50")
  response <- httr2::req_perform(request)
  report_request_result(request, response, ";CMS Query", query,  ";t get_bo_item 135")
  if (response$status_code==200) {
    results <- bind_bo_query_results_to_tibble(httr2::resp_body_json(response)$entries)
    if (nrow(results) > 1) {
      results <- dplyr::distinct(results, SI_ID, .keep_all=TRUE)
    }
  }
  return(results)
}

paste_url <- function(...) {
  x <- list(...)
  n <- names(x)
  if (length(n) > 0)
    x <- Map(list, n, x) %>% unlist()
  x <- as.list(x) %>% keep(str_length(.) > 0)
  x %>% do.call(what = (function(...)
    paste(..., sep = "/")))
}

quiet_is_numeric <- function(x) {
  if (is_atomic(x)) {
    result <- is_null_or_empty(x) || purrr::quietly(function(x)!is.na(as.numeric(x)))(x)$result
    return (result)
  }
  return(FALSE)
}

flatten_scalars <- function(list, stop_name = NULL) {
  # recursiviely unwrap lists with one named item
  while (length(names(list)) == 1 && (is_null(stop_name) || !stop_name %in% names(list)))
    list %<>% purrr::pluck(names(list))
  list
}

bind_list <- function(list) {
  if (length(names(list)) > 1) {
    list %>% bind_rows()
  } else {
    list %>% purrr::map(purrr::flatten_dfr) %>% bind_rows()
  }
}

bind_result <- function(result, stop_name = NULL) {
  result %<>% flatten_scalars(stop_name) %>%
    bind_list()
}

return_bo_response_content <- function(response) {
  if (response$headers[['content-type']] %>% str_detect('json')) {
    result <- httr2::resp_body_json(response, simplifyVector = TRUE)
    if (length(names(result)) == 1) {
      result <- flatten_scalars(result)
    } else {
      result <- result %>% bind_rows()
    }
    result
  } else if (response$headers[['content-type']] %>% str_detect('text')) {
    httr2::resp_body_string(response)
  } else {
    # otherwise some kind of raw content
    httr2::resp_body_raw(response)
  }
}

request_bo_raylight_endpoint <- function(conn, ..., query, accept=NULL, body = NULL, method = NULL) {
  request <- check_bo_connection(conn)
  # wrap request so it can be modified inside of lapply
  ref <- new_bo_request_reference(conn)

  ref$request %<>% httr2::req_url_path_append("raylight/v1")

  path <- list(...)
  names(path) %>% lapply(function(x)ref$request %<>% httr2::req_url_path_append(x,path[[x]]))

  if (!is_null_or_empty(query)) {
    ref$request %<>% httr2::req_url_query(!!!query)
  }
  if (!is_empty(accept)) {
    ref$request %<>% httr2::req_headers('Accept'=accept)
  }
  if (!is.null(body)) {
    ref$request %<>% httr2::req_body_json(body)
    ref$request %<>% httr2::req_method(method='PUT')
  }
  if (!is.null(method)) {
    ref$request %<>% httr2::req_method(method=method)
  }
  response <- httr2::req_perform(ref$request)
  report_request_result(request, response, ";Children", "request_bo_raylight_endpoint 197")
  return_bo_response_content(response)
}


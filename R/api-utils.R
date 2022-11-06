#' Report error in API call
#'
#' @param request httr::request
#' @param response httr::request
#' @param ... list of text arguments for message
#'
#' @noRd
report_api_error <- function(request, response, ...) {
  inputs <- list(...)
  message1 <- paste(head(inputs, 1), collapse = " ")
  message2 <- paste(tail(inputs, -1), collapse = " ")
  # if(is.na(message1)||message1=='NA') #browser()()
  if (response$status_code == 200) {
    mycat(paste(message1, "succeeded", ";", message2, response$url))
  } else {
    mycat(paste(message1, "failed", response$status_code, ";", message2, response$url, request$verb, ";", content(response, as='text', encoding='UTF-8')), file = stderr())
    # throw()
  }
}

#' Return Id from item properties or from numeric value
#'
#' @param item Item properties as tibble or item id as numeric value
#'
#' @return Numeric id of item

get_bo_item_id <- function(item) {
  if (is_list(item) || is_tibble(item) || is.environment(item)) {
    return(item$SI_ID)
  }
  if (testNumericArgument(item)) {
    return(item)
  }
  return(NULL)
}

#' Title
#'
#' @param conn Connection Reference
#' @param id Numeric id of item
#'
#' @return Item as a tibble of properties
#' @export
get_bo_item_from_id <- function(conn, id) {
  request <- check_bo_connection(conn = conn)
  query <- paste0("SELECT * FROM CI_INFOOBJECTS WHERE SI_ID=", id)
  body <- list("query" = query) %>% listToJSON()
  url <- paste0(request$url, "/v1/cmsquery?page=1&pagesize=140")
  response <- POST(url = url, body = body, request)
  report_api_error(request, response, ";CMS Query", "get_bo_item_from_id 22")
  result <- content(response)$entries # %>% lapply(function(x)as_tibble(x))
  result[[1]]
}

append_query_conditions <- function(query, name = NULL, folder_id = NULL, kind = NULL, owner = NULL) {
  addAnd <- function(needAnd) {
    if (needAnd) {
      return(' AND')
    }
    return('')
  }

  needAnd = FALSE
  if (hasArg("name") && !is.null(name) && str_length(name) > 0) {
    query <- paste0(query, " SI_NAME='", name, "'")
    needAnd = TRUE
  }
  if (hasArg("folder_id") && !is.null(folder_id) && folder_id > 0) {
    query <-
      paste0(query, addAnd(needAnd), " SI_PARENT_FOLDER=", folder_id)
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

# Vectorize get_bo_item_from_id for use in mutate
v_get_bo_item_from_id<- Vectorize(get_bo_item_from_id, c("id"), SIMPLIFY = FALSE)

sort_bo_items_by_date <- function(items) {
  if (nrow(items)<=1) return(items)
  items %<>% mutate(item = v_get_bo_item_from_id(conn = request, SI_ID))
  items$parent <-
    items$item %>% lapply(function(x)
      x$SI_PARENT_FOLDER)
  # get the format for a string with stamp(items$SI_UPDATE_TS)
  mycat(";Document Dates", paste(c(list(items$SI_NAME))), paste(c(list(
    items$SI_UPDATE_TS
  ))))
  items %<>% mutate(pdate = parse_date_time2(SI_UPDATE_TS, "%Om %d, %Y %H:%M %Op"))
  return(items %<>% arrange(pdate))
}

bind_bo_query_results_to_tibble <- function(entries) {
  items <- entries %>%
    lapply(function(x) {
      as_tibble(x)
    }) %>%
    bind_rows()
  return(items)
}

#' Retreive BO item by name
#'
#' @param conn Connection reference
#' @param name Name of the item
#' @param folder_id Numeric id of the parent folder
#' @param kind kind of item (optional)
#' @param owner owner of item (optional)
#'
#' @return Tibble of item properties
#' @export
get_bo_item_from_name <- function(conn, name, folder_id, kind = NULL, owner = NULL) {
  request <- check_bo_connection(conn)
  query <- paste0(
    "SELECT SI_KIND, SI_ID, SI_PARENT_FOLDER, SI_NAME, SI_OWNER, SI_CREATION_TIME, SI_UPDATE_TS, SI_PATH FROM CI_INFOOBJECTS WHERE"
  )
  query <- append_query_conditions(query, name, folder_id, kind, owner)
  body <- list("query" = query) %>% listToJSON()
  url <- paste0(request$url, "/v1/cmsquery?page=1&pagesize=50")
  response <- POST(url = url, body = body, request)
  report_api_error(request,
                response,
                ";CMS Query",
                query,
                "get_bo_item_from_name 162")
  return(bind_bo_query_results_to_tibble(content(response)$entries))
}

#' Retrieve BO object from an matching pattern
#'
#' @param conn Connection reference
#' @param pattern glob pattern for item name matching
#' @param folder_id Numeric id of the containing folder
#' @param kind kind of item (optional)
#' @param owner owner of item (optional)
#'
#' @return Tibble of item properties
#' @export
get_bo_item_from_pattern <- function(conn, pattern, folder_id = NULL, kind = NULL, owner = NULL) {
  request <- check_bo_connection(conn)
  query <- paste0(
    "SELECT SI_KIND, SI_ID, SI_NAME, SI_OWNER, SI_CREATION_TIME FROM CI_INFOOBJECTS WHERE SI_NAME LIKE '",
    pattern,
    "'"
  )
  query <- append_query_conditions(query, folder_id, kind, owner)
  body <- list("query" = query) %>% listToJSON()
  url <- paste0(request$url, "/v1/cmsquery?page=1&pagesize=50")
  response <- POST(url = url, body = body, request)
  report_api_error(request,
                response,
                ";CMS Query",
                query,
                "get_bo_item_from_pattern 190")
  bind_bo_query_results_to_tibble(content(response)$entries)
}

add_bo_headers <- function(headers) {
  headers[["Accept"]] <- "application/json"
  headers[["Accept-Encoding"]] <- "gzip, deflate"
  headers[["Content-Type"]] <- "multipart/form-data"
  return(headers)
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

flatten_scalars <- function(x) {
  # recursiviely unwrap lists with one named item
  while (length(names(x)) == 1)
    x %<>% pluck(names(x))
  x
}

return_bo_response_content <- function(response) {
  if (response$headers[['content-type']] %>% str_detect('json')) {
    # fromJSON worked better here than the json paraser in httr
    result <- content(response, as = "text", encoding = "UTF-8") %>% fromJSON(simplifyVector = TRUE, simplifyDataFrame = TRUE)
    result %>% flatten_scalars()
  } else if (response$headers[['content-type']] %>% str_detect('text')) {
    content(response, as = "text", encoding = "UTF-8")
  } else {
    # otherwise some kind of raw content
    content(response, as = "raw")
  }
}

GET_bo_raylight_endpoint <- function(conn, ..., querystring) {
  request <- check_bo_connection(conn)
  url <- paste_url(request$url, "raylight/v1", ...)

  if (!isNullOrEmpty(querystring)) {
    url <- paste0(url, querystring)
  }
  response <- GET(url = url, request)
  report_api_error(request, response, ";Children", "GET_bo_raylight_endpoint 197")
  return_bo_response_content(response)
}

PUT_bo_raylight_endpoint <- function(conn, ..., querystring, body = NULL) {
  request <- check_bo_connection(conn)
  url <- paste_url(request$url, "raylight/v1", ...)

  if (!isNullOrEmpty(querystring)) {
    url <- paste0(url, querystring)
  }
  if (!is.null(body) &&
      !is.character(body) &&
      request$headers[['Content-Type']] %>% str_detect('json')) {
    body <- body %>% toJSON()
  }
  response <- PUT(url = url, body = body, request)
  report_api_error(request, response, ";Children", "PUT_bo_raylight_endpoint 214")
  return_bo_response_content(response)
}


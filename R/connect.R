#' @importFrom methods hasArg new
#' @importFrom methods hasArg new
#' @importFrom utils head tail timestamp
#' @importFrom stats filter
#' @importFrom magrittr %<>% %>%
#' @importFrom httr GET POST PUT DELETE add_headers upload_file content config
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom getPass getPass
#' @importFrom RSQLite dbConnect dbDisconnect dbExecute dbListTables dbReadTable dbWriteTable dbAppendTable
#' @include api-utils.R
#'
# Create reference class for httr::request
setOldClass("request")
setRefClass("request_reference_class", fields = list(request = "request"))
new_bo_request_reference <- setRefClass("request_reference_class", fields = list(request = "request"))

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

get_home_path <- function() {
  home_path <- path.expand('~')
  return(home_path)
}

get_keyring_path <- function() {
  home_path <- get_home_path()
  keyring_path <- file.path(home_path, Sys.getenv("BO_KEYRING_FILE_PATH"))
  return(keyring_path)
}

#' Title
#'
#' @param name Secret name
#' @param value Secret value
#'
#' @export
set_keyring_secret <- function(name, value) {
  options(keyring_backend = "file")
  keyring_path = get_keyring_path()
  kr <- keyring::backend_file$new()
  keyring_path <- file.path(keyring_path)
  if (!file.exists(keyring_path)) {
    mycat(paste(";Creating new keyring at", keyring_path), "set_keyring_secret 63")
    kr$keyring_create("system", password = Sys.getenv("BO_KEYRING_PASSWORD"))
  }
  kr$keyring_unlock(password = Sys.getenv("BO_KEYRING_PASSWORD"))
  # username below is the keyring entry name
  kr$set_with_value(Sys.getenv("BO_KEYRING_SERVICE_NAME"), username = name, password = value)
  mycat(";set_keyring_secret 62", name, file = stderr())
}

#' Title
#'
#' @param name Secret name
#'
#' @return Secret value
#' @export

get_keyring_secret <- function(name) {
  options(keyring_backend = "file")
  kr <- keyring::backend_file$new()
  keyring_path <- get_keyring_path()
  service_name <- Sys.getenv("BO_KEYRING_SERVICE_NAME")
  if (!file.exists(keyring_path)) {
    mycat(paste(";Creating new keyring at", keyring_path), "get_keyring_secret 77")
    kr$keyring_create("system", password = Sys.getenv("BO_KEYRING_PASSWORD"))
  }
  kr$keyring_unlock(password = Sys.getenv("BO_KEYRING_PASSWORD"))

  mycat(";", "keyring location", keyring_path, "keyring count", nrow(kr$list()))
  secret <- tryCatch(
    {
      mycat(";get_keyring_secret 85", "getting secret", name)
      kr$get(service_name, username = name)
    },
    error = function(cond) {
    }
  )
  if (is.null(secret)) {
    if (interactive()) {
      prompt <- paste("Secret value for", name, ':')
      kr$set(service_name, username = name, prompt = prompt)
      secret <- kr$get(service_name, username = name)
    }
  }
  if (is.null(secret)) {
    mycat("Missing entry ", name, " for service", service_name, 'at path',
          keyring_path, "get_keyring_secret 101")
  } else {
    mycat(";getBOSecret 104 secret length", str_length(secret))
  }
  return(secret)
}

listToJSON <- function(list) {
  list %>%
    toJSON() %>%
    gsub("(\\[|])", "", .)
}

#' Title Check to see if token is still valid and get a new token if needed
#'
#' @param server Server name and port to connect to
#' @param conn connection reference
#'
#' @return
#' @export
check_bo_connection_state <- function(conn, server, extra_test = FALSE) {
  if (hasArg("conn") && !is.null(conn) && !is.null(conn$request)) {
    request <- conn$request
    if (server == request$headers[["Host"]] && !is.null(request$url)) {
      httr::set_config(config(ssl_verifypeer = 0L))
      url <- str_replace(request$url, "biprws", "dswsbobje/services/Session")
      response <- GET(url = url, request)
      if (response$status_code == 200) {
        if (extra_test) {
          body <- '{"query" = "SELECT SI_ID FROM CI_INFOOBJECTS WHERE SI_ID=1"}'
          url <- paste0(request$url, "/v1/cmsquery?page=1&pagesize=50")
          response <- POST(url = url, body = body, request)
          if (response$status_code != 401) {
            mycat(";Existing connection extra test succeeded", ";testBORequest 88", conn$request$url, response$status_code, content(response, as = "text", encoding = "UTF-8"))
            return(TRUE)
          }
        } else {
          mycat(";Existing connection test succeeded", ";testBORequest 88", conn$request$url, response$status_code, content(response, as = "text", encoding = "UTF-8"))
          return(TRUE)
        }
      }
    }
  }
  return(FALSE)
}

try_token <- function(conn, server, token) {
  cat(";Trying token from database (2)", token)
  tryCatch({
      conn$request$headers[["X-SAP-LogonToken"]] <- token
      result <- check_bo_connection_state(conn = conn, server = server)
    }, error = {
      result <- NULL
  })
  if (result) {
    return(conn)
  }
  return(NULL)
}

check_bo_connection <- function(conn, server=Sys.getenv("BO_SERVER")) {
  if (rlang::is_empty(conn)) {
    conn <- open_bo_connection(server)
  }
  if (class(conn) == "request_reference_class") {
    return(conn$request)
  } else {
    return(conn)
  }
}

find_valid_token <- function(conn, server, username) {
  tokens <- get_cached_bo_tokens(username, server)$token
  if (!is.null(tokens) && length(tokens) > 0) {
    for (i in 1:length(tokens)) {
      token <- tokens[i]
      newconn <- try_token(conn = conn, server = server, token = token)
      if (!is.null(newconn)) {
        mycat('Using token', token)
        return(newconn)
      } else {
        cat("try_token", "token", token, "failed, removing token")
        remove_cached_bo_token(token)
      }
    }
  }
  return(NULL)
}

#' Title
#'
#' @param server Server to connect to "server:port"
#' @param conn Connection reference to reuse
#'
#' @return Connection reference
#' @export
#'
open_bo_connection <- function(server=Sys.getenv("BO_SERVER"), conn = NULL) {
  if (check_bo_connection_state(conn=conn, server=server)) {
    return(conn)
  }
  if (!hasArg("conn") || is.null(conn)) {
    conn <- new_bo_request_reference()
  }
  conn$request <- add_headers("Accept" = "application/json", "Content-Type" = "application/json", "Host" = server)
  conn$request$url <- paste0("https://", server, "/biprws")
  username <- Sys.getenv("BO_USERNAME")
  newconn <- find_valid_token(conn, server, username)
  if (!is.null(newconn)) {
    return(newconn)
  }
  conn$request <- add_headers("Accept" = "application/json", "Content-Type" = "application/json", "Host" = server)
  conn$request$url <- paste0("https://", server, "/biprws")
  keyring_entry <- Sys.getenv("BO_USER_PASSWORD_KEYRING_ENTRY")
  pwd <- get_keyring_secret(keyring_entry)
  body <- list(
    "clienttype" = "my_BI_Application",
    "username" = username,
    "password" = pwd,
    "auth" = "secWinAD"
  ) %>% listToJSON()
  url <- paste0(conn$request$url, "/v1/logon/long")
  httr::set_config(config(ssl_verifypeer = 0L))
  response <- POST(url = url, body = body, conn$request)
  report_api_error(conn$request, response, "New connection", "open_bo_connection 146")
  # browser()
  token <- content(response)$logontoken
  if (!is.null(token) && str_length(token) > 0) {
    conn$request$headers[["X-SAP-LogonToken"]] <- token
    save_bo_token(username, server, token)
  } else {
    throw.error("Logon failed")
  }
  mycat('Using token', token)
  conn
}

get_bo_connection_database_path <- function() {
  return(file.path(get_home_path(), "/bo_connections.sqlite"))
}

#' Close connectionto BO server
#'
#' @param conn Connection reference to close
#'
#' @return Connection reference
#' @export
close_bo_connection <- function(conn) {
  request <- check_bo_connection(conn)
  url <- paste0(conn$request$url, "/logoff")
  response <- POST(url = url, request = request)
  report_api_error(request, response, ";Disconnect", "logOffBOSession 129")
  conn
}

#' Clear all saved OAuth tokens
#'
#' @export
#'
clear_saved_tokens <- function() {
  conn <- dbConnect(RSQLite::SQLite(), dbname = get_bo_connection_database_path())
  dbExecute(conn, paste0("DELETE FROM tokens"))
  mycat("Cleared saved tokens")
  dbDisconnect(conn)
}

get_cached_bo_tokens <- function(username, server) {
  conn <- dbConnect(RSQLite::SQLite(), dbname = get_bo_connection_database_path())
  table <- dbListTables(conn) %>% keep(~ .x == "tokens")
  if (!(length(table) > 0)) {
    tokens <- tibble(timestamp = Date(0), usernamekey = character(0), serverkey = character(0), token = character(0))
    dbWriteTable(conn, "tokens", tokens, append = TRUE)
  } else {
    tokens <- dbReadTable(conn, "tokens")
  }
  dbDisconnect(conn)
  filteredtokens <- tokens %>%
    dplyr::filter(serverkey == server) %>%
    dplyr::filter(usernamekey == username) %>%
    dplyr::arrange(timestamp)
  return(filteredtokens)
}

remove_cached_bo_token <- function(token) {
  mycat(";removing token", token)
  conn <- dbConnect(RSQLite::SQLite(), dbname = get_bo_connection_database_path())
  dbExecute(conn, paste0("DELETE FROM tokens WHERE token='", token, "'"))
  dbDisconnect(conn)
}

save_bo_token <- function(username, server, token) {
  conn <- dbConnect(RSQLite::SQLite(), dbname = get_bo_connection_database_path())
  tokens <- tibble(timestamp = now(), usernamekey = username, serverkey = server, token = token)
  table <- dbListTables(conn) %>% keep(~ .x == "tokens")
  if (length(table) > 0) {
    dbAppendTable(conn, "tokens", tokens)
  } else {
    dbWriteTable(conn, "tokens", tokens, append = TRUE)
  }
  dbDisconnect(conn)
}


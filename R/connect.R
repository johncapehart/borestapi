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
# prerequisites ----------------------------------------------------------------

# Create reference class for httr::request
setOldClass("request")
setRefClass("request_reference_class", fields = list(request = "request"))
new_bo_request_reference <- setRefClass("request_reference_class", fields = list(request = "request"))

get_home_path <- function() {
  home_path <- path.expand('~')
  return(home_path)
}

# password storage and retrieval ----------------------------------------------------------------

get_keyring_path <- function() {
  home_path <- get_home_path()
  keyring_path <- file.path(home_path, Sys.getenv("BO_KEYRING_FILE_PATH"))
  return(keyring_path)
}

get_keyring_password <- function() {
  user = Sys.getenv('USER')
  return(digest::digest(paste0('salt', user), 'sha1'))
}

#' Delete the keyring
#'
#' @export
clear_keyring <- function() {
  mycat(paste("Removing keyring at", get_keyring_path()), "clear_keyring line 41")
  file.remove(get_keyring_path())
}

create_keyring_file <- function(kr) {
  mycat(paste(";Creating new keyring at", get_keyring_path()), "create_keyring_file line 46")
  kr$keyring_create('system', password = get_keyring_password())
  return(kr)
}

get_keyring <- function() {
  options(keyring_backend = "file")
  kr <- keyring::backend_file$new()
  keyring_path <- get_keyring_path()
  if (!file.exists(keyring_path)) {
    mycat("get_keyring keyring file not found at", get_keyring_path(), "get_keyring ilne 56")
    kr <- create_keyring_file(kr)
  }
  kr
}

unlock_keyring <- function() {
  kr <- get_keyring()
  tryCatch({
      kr$keyring_unlock(password = get_keyring_password())
      mycat(";", "unlock_keyring keyring location", get_keyring_path(), "is locked:", kr$keyring_is_locked(), "keyring count", nrow(kr$list()), "unlock_keyring line 66")
  }, error=function(cond){
      mycat("Could not unlock keyring at", get_keyring_path(), ";unlock_keyring line 68")
      clear_keyring()
      kr <- get_keyring()
      kr$keyring_unlock(password = get_keyring_password())
    })
   return(kr)
}

#' Set a secret in the keyring
#'
#' @param name Secret name
#' @param value Secret value
#'
#' @export
set_keyring_secret <- function(name, value) {
  kr <- unlock_keyring()
  # username below is the keyring entry name
  kr$set_with_value('borestapi', username = name, password = value)
  mycat(";set_keyring_secret 62", name, file = stderr())
}

#' Get a secret from the keyring
#'
#' @param name Secret name
#'
#' @return Secret value
#' @export

get_keyring_secret <- function(name) {
  keyring_path <- get_keyring_path()
  service_name <- 'borestapi'
  kr <- unlock_keyring()
  secret <- tryCatch(
    {
      mycat(";get_keyring_secret 101", "getting secret", name)
      kr$get(service_name, username = name)
    },
    error = function(cond) {
    }
  )
  if (is.null(secret)) {
    if (interactive()) {
      # if interactive we can prompt for value
      prompt <- paste("Secret value for", name, ':')
      kr$set(service_name, username = name, prompt = prompt)
      secret <- kr$get(service_name, username = name)
    }
  }
  if (is.null(secret)) {
    mycat("Missing entry ", name, " for service", service_name, 'at path',
          keyring_path, "get_keyring_secret line 118")
  } else {
    mycat("Retrived keyring entry", name, "secret length", str_length(secret) , ";get_keyring_secret line 119")
  }
  return(secret)
}

# connection validation ----------------------------------------------------------------

listToJSON <- function(list) {
  list %>%
    toJSON() %>%
    gsub("(\\[|])", "", .)
}

#' Check to see if an SAP access token is still valid
#'
#' @param server Server name and port to connect to
#' @param conn connection reference
#'
#' @return TRUE if connection is valid, FALSE otherwise
check_bo_connection_state <- function(conn, server, extra_test = FALSE) {
  if (hasArg("conn") && !is.null(conn) && !is.null(conn$request)) {
    request <- conn$request # request is the mutable object referred to by the reference
    if (server == request$headers[["Host"]] && !is.null(request$url)) {
      httr::set_config(config(ssl_verifypeer = 0L)) # skip certificate check
      url <- str_replace(request$url, "biprws", "dswsbobje/services/Session") # url for session information
      response <- tryCatch({
          GET(url = url, request)
        },
        error = function(cond) {
          mycat("GET Test of connection failed", url, cond, ";testBORequest line 141")
          NULL
      })
      if (!is_empty(response) && response$status_code == 200) { # received valid session data
        mycat("Test of connection succeeded", conn$request$url, response$status_code, content(response, as = "text", encoding = "UTF-8"), ";testBORequest line 153")
        if (extra_test) { # run a second test
          body <- '{"query" = "SELECT SI_ID FROM CI_INFOOBJECTS WHERE SI_ID=1"}'
          url <- paste0(request$url, "/v1/cmsquery?page=1&pagesize=50")
          response <- POST(url = url, body = body, request)
          if (response$status_code != 401) {
            mycat("POST Test of connection succeeded", url, response$status_code, content(response, as = "text", encoding = "UTF-8"), ";testBORequest line 149")
            return(TRUE)
          }
        } else {
          return(TRUE)
        }
      }
    }
  }
  return(FALSE)
}

try_token <- function(conn, server, token) {
  cat(";Trying token from database (2)", token)
  result <- tryCatch({
      conn$request$headers[["X-SAP-LogonToken"]] <- token
      check_bo_connection_state(conn = conn, server = server)
    }, error = function(cond) {
      FALSE
  })
  return(result)
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

#' Find a valid token matching server and username
#'
#' @param conn Connection reference
#' @param server Server name and port
#' @param username Username
#'
#' @return TRUE if token found; FALSE otherwise
get_cached_token <- function(conn, server, username) {
  tokens <- get_saved_tokens(username, server)$token # get matching tokens
  if (!is.null(tokens) && length(tokens) > 0) {
    for (i in 1:length(tokens)) {
      token <- tokens[i]
      if (try_token(conn = conn, server = server, token = token)) {
        mycat('Using token', conn$request$headers[["X-SAP-LogonToken"]])
        mycat("reusing token ================================================================")
        return(TRUE)
      } else {
        mycat("removing token ----------------------------------------------------------------")
        cat("try_token", "token", token, "failed, removing token")
        remove_cached_bo_token(token)
      }
    }
  }
  return(FALSE)
}

get_new_token <- function(conn, server, username, password = NULL) {
  # get password from keyring
  keyring_entry <- 'business objects password'
  if (is_empty(password)) {
    password <- get_keyring_secret(keyring_entry)
  }
  # get request body as json
  body <- list(
    "clienttype" = "my_BI_Application",
    "username" = username,
    "password" = password,
    "auth" = "secWinAD"
  ) %>% listToJSON()
  url <- paste0(conn$request$url, "/v1/logon/long") # append longon endpoint to url
  response <- POST(url = url, body = body, conn$request)
  report_api_error(conn$request, response, "New connection", "open_bo_connection line 146")
  token <- content(response)$logontoken
  if (!is.null(token) && str_length(token) > 0) {
    conn$request$headers[["X-SAP-LogonToken"]] <- token
    save_bo_token(username, server, token)
  } else {
    stop(paste("Logon to ", server, "as", username, "failed"))
  }
  mycat("new token ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
  mycat('Using new token', token, ";open_bo_connection line 259")
  return(TRUE)
}

# connection management ----------------------------------------------------------------

#' Open a connection to a BO server
#'
#' @param server Server to connect to "server:port"
#' @param conn Connection reference to reuse (optional)
#'
#' @return Connection reference
#' @export
#'
open_bo_connection <- function(server = Sys.getenv("BO_SERVER"),
           username = Sys.getenv("BO_USERNAME"),
           password = NULL,
           conn = NULL) {
  if (check_bo_connection_state(conn = conn, server = server)) {
    return(conn)
  }
  if (!hasArg("conn") || is.null(conn)) {
    conn <- new_bo_request_reference() # make a new empty request reference
  }
  httr::set_config(config(ssl_verifypeer = 0L)) # skip certificate checks
  # set request headers
  conn$request <-
    add_headers("Accept" = "application/json",
                "Content-Type" = "application/json",
                "Host" = server)
  # set base url
  conn$request$url <- paste0("https://", server, "/biprws")
  username <- Sys.getenv("BO_USERNAME")
  if (get_cached_token(conn, server, username)) {
    # search for valid token matching server and username
    return(conn)
  }
  if (get_new_token(conn, server, username, password)) {
    return(conn)
  }
  stop("open_bo_connectin failed")
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
  report_api_error(request, response, ";Disconnect", "logOffBOSession line 129")
  conn
}

# token caching ----------------------------------------------------------------

get_bo_token_database_path <- function() {
  return(file.path(get_home_path(), "/bo_connections.sqlite"))
}

#' Clear all saved tokens
#'
#' @export
clear_saved_tokens <- function() {
  conn <- dbConnect(RSQLite::SQLite(), dbname = get_bo_token_database_path())
  dbExecute(conn, paste0("DELETE FROM tokens"))
  mycat("Cleared saved tokens")
  dbDisconnect(conn)
}

get_saved_tokens <- function(username, server) {
  conn <- dbConnect(RSQLite::SQLite(), dbname = get_bo_token_database_path())
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
  mycat("get_saved_tokens token count", nrow(filteredtokens), "get_saved_tokens line 328")
  return(filteredtokens)
}

remove_cached_bo_token <- function(token) {
  mycat("removing token", token, ";remove_cached_bo_token line 316")
  conn <- dbConnect(RSQLite::SQLite(), dbname = get_bo_token_database_path())
  dbExecute(conn, paste0("DELETE FROM tokens WHERE token='", token, "'"))
  mycat("token removed", token, "count", nrow(dbReadTable(conn, "tokens")), ";remove_cached_bo_token line 335")
  dbDisconnect(conn)
}

save_bo_token <- function(username, server, token) {
  conn <- dbConnect(RSQLite::SQLite(), dbname = get_bo_token_database_path())
  tokens <- tibble(timestamp = now(), usernamekey = username, serverkey = server, token = token)
  table <- dbListTables(conn) %>% keep(~ .x == "tokens")
  if (length(table) > 0) {
    dbAppendTable(conn, "tokens", tokens)
  } else {
    dbWriteTable(conn, "tokens", tokens, append = TRUE)
  }
  mycat("token saved", token, "count", nrow(dbReadTable(conn, "tokens")), ";save_bo_token line 316")
  dbDisconnect(conn)
}


#' @importFrom methods hasArg new
#' @importFrom methods hasArg new
#' @importFrom utils head tail timestamp
#' @importFrom stats filter
#' @importFrom magrittr %<>% %>%
#' @importFrom httr GET POST PUT DELETE add_headers upload_file content config
#' @importFrom jsonlite toJSON fromJSON
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

# connection validation ----------------------------------------------------------------

listToJSON <- function(list) {
  list %>%
    toJSON() %>%
    gsub("(\\[|])", "", .)
}

#' Check to see if an SAP access token is still valid
#'
#' @param conn connection reference
#' @param server Server name and port to connect to

#' @return TRUE if connection is valid, FALSE otherwise
check_bo_connection_state <- function(conn, server, extra_test = TRUE) {
  if (hasArg("conn") && !is.null(conn) && !is.null(conn$request)) {
    request <- conn$request # request is the mutable object referred to by the reference
    if (server == request$headers[["Host"]] && !is.null(request$url)) {
      # a POST query of item SI_ID = 1 is the test of a token
      httr::set_config(config(ssl_verifypeer = 0L)) # skip certificate check
      body <- '{"query" = "SELECT SI_ID FROM CI_INFOOBJECTS WHERE SI_ID=1"}'
      url <- paste0(request$url, "/v1/cmsquery?page=1&pagesize=50")
      response <- POST(url = url, body = body, request)
      if (response$status_code != 401) {
        mycat("POST Test of connection succeeded", url, response$status_code, content(response, as = "text", encoding = "UTF-8"), ";testBORequest line 149")
        return(TRUE)
      } else {
        mycat("POST Test of connection failed", url, response$status_codell, ";testBORequest line 161")
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

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

#' Get user rights on the BO server
#'
#' @param conn connection reference
#'
#' @return Response content or NULL
#' @export
get_user_rights <- function(conn) {
  # a POST query of item SI_ID = 1 is the test of a token
  httr::set_config(config(ssl_verifypeer = 0L)) # skip certificate check
  url = paste0(conn$request$url, '/raylight/v1/session/rights')
  response <- GET(url, conn$request)
  if (response$status_code == 200) {
    return(content(response))
  }
  return(NULL)
}

#' Check to see if an SAP access token is still valid
#'
#' @param conn connection reference
#' @param server Server name and port to connect to
#'
#' @return TRUE if connection is valid, FALSE otherwise
#' @export
#' @noRd
check_bo_connection_state <- function(conn, server) {
  if (hasArg("conn") && !is.null(conn) && !is.null(conn$request)) {
    if (server == conn$request$headers[["Host"]] && !is.null(conn$request$url)) {
      rights <- get_user_rights(conn)
      return(!is_empty(rights))
    }
  }
  return(FALSE)
}

try_token <- function(conn, server, token) {
  cat(";Trying token from database (2)", token)
  result <- tryCatch({
      # this sets the token in the mutable connection
      conn$request$headers[["X-SAP-LogonToken"]] <- token
      check_bo_connection_state(conn = conn, server = server)
      # TODO:remove the token header
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
#' @noRd
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
        remove_cached_token(token)
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
  report_request_result(conn$request, response, "New connection", "open_bo_connection line 146")
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

get_new_request <- function(conn, server) {
  if (!hasArg("conn") || is.null(conn)) {
    conn <- new_bo_request_reference() # make a new empty request reference
  }
  # set request headers
  conn$request <-
    add_headers("Accept" = "application/json",
                "Content-Type" = "application/json",
                "Host" = server)
  # set base url
  conn$request$url <- paste0("https://", server, "/biprws")
  conn
}
#' Open a connection to a BO server. You can
#'
#' @param server Server to connect to "server:port". Defaults BO_SERVER environment variable
#' @param conn Connection reference to reuse (optional)
#' @param username (optional). Defaults to "BO_USERNAME environment variable
#' @param password (optional). Use getPass() to enter value
#' @return Connection reference
#' @examples
#' # open a connection to the server in environment variable "BO_SERVER" using user in "BO_USERNAME". and password in a
#' # keyring at "BO_KEYRING_FILE_PATH"
#' open_bo_connection()
#' # open a connection to a server using username and password
#' open_bo_connection(Sys.getenv("BO_SERVER"), username = 'john.capehart', password=getPass())
#' @export
open_bo_connection <- function(server = Sys.getenv("BO_SERVER"),
           username = Sys.getenv("BO_USERNAME"),
           password = NULL,
           conn = NULL) {
  httr::set_config(config(ssl_verifypeer = 0L)) # skip certificate checks
  # reset request
  conn <- get_new_request(conn, server)
  if (get_cached_token(conn, server, username)) {
    # search for valid token matching server and username
    return(conn)
  }
  # reset request
  conn <- get_new_request(conn, server)
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
  report_request_result(request, response, ";Disconnect", "logOffBOSession line 129")
  conn
}

#' @importFrom httr2 request
#' @importFrom utils head tail
#' @importFrom methods new
#' @include api-utils.R
#'
# prerequisites ----------------------------------------------------------------

# Create reference class for httr2::request

setOldClass("httr2_request")
setRefClass("request_reference_class", fields = list(request='httr2_request'))
new_bo_request_reference <- setRefClass("request_reference_class", fields = list(request = "httr2_request"))

# connection validation ----------------------------------------------------------------

#' Get user rights on the BO server
#'
#' @param conn connection reference
#'
#' @return Response content or NULL
#' @noRd
get_user_rights <- function(request) {
  request %<>% httr2::req_url_path_append('raylight/v1/session/rights')
  response <- httr2::req_perform(request)
  if (httr2::resp_status(response) == 200) {
    return(httr2::resp_body_json(response))
  }
  return(NULL)
}

#' Check to see if an SAP access token is still valid
#'
#' @param conn connection reference
#' @param server Server name and port to connect to
#'
#' @return TRUE if connection is valid, FALSE otherwise
#' @noRd
check_bo_connection_state <- function(request) {
  request %<>% httr2::req_error(is_error = function(resp) FALSE)
  rights <- get_user_rights(request)
    return(!is_empty(rights))
}

try_token <- function(conn, server, token) {
  logger::log_info("Trying token from database", ";d", "{token}", ";;try_token line 68")
  result <- tryCatch({
      # this sets the token in the mutable connection
      request <- conn$request %>% httr2::req_headers("X-SAP-LogonToken" = token)
      if (check_bo_connection_state(request)) {
        conn$request %<>% httr2::req_headers("X-SAP-LogonToken" = token)
        TRUE
      } else {
        FALSE
      }
    }, error = function(cond) {
      FALSE
  })
  return(result)
}

check_bo_connection <- function(conn) {
  if (rlang::is_empty(conn)) {
    logger::log_error("Empty connection reference",";t check_bo_connection 83")
  }
  if (methods::is(conn,'request_reference_class')) {
    if (check_bo_connection_state(conn$request)) {
      return(conn$request)
    } else {
      reconnect_bo_connection(conn)
      return(conn$request)
    }
  }
  stop(paste("check_bo_connection failed"))
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
  tokens <- get_saved_items(username, server)$value # get matching tokens
  if (!is_empty(tokens) && length(tokens) > 0) {
    for (i in 1:length(tokens)) {
      token <- tokens[i]
      if (try_token(conn = conn, server=server, token = token)) {
        log_with_separator("Reusing token for ", paste(server, username), separator='=', width=120)
        log_debug(paste0("{token}"))
        return(TRUE)
      } else {
        log_with_separator("Removing token for ", paste(server, username), separator='-', width=120)
        log_debug(paste0("{token}"))
        remove_item(username, server, table_name = get_token_table_name())
      }
    }
  }
  return(FALSE)
}

get_new_token <- function(conn, server, username, password = NULL) {
  #browser()
  body <- list(
    "clienttype" = "my_BI_Application",
    "username" = username,
    "password" = password,
    "auth" = "secWinAD"
  )
  request <- conn$request
  request %<>% httr2::req_headers('X-SAP-LogonToken'='')
  request %<>% httr2::req_url_path_append( 'v1/logon/long') # append longon endpoint to url
  request %<>% httr2::req_body_json(body)
  response <- httr2::req_perform(request)
  report_request_result(conn$request, response, "New connection", "open_bo_connection line 146")
  token <- httr2::resp_body_json(response)$logontoken
  if (!is.null(token) && stringr::str_length(token) > 0) {
    conn$request %<>% httr2::req_headers("X-SAP-LogonToken" = token)
    save_item(username, server, token, table_name = get_token_table_name())
    log_with_separator("New token for ", paste(server, username), separator='+', width=120)
    log_debug(paste0("{token}"))
  } else {
    stop(paste("Logon to ", server, "as", username, "failed"))
  }
  return(TRUE)
}

# connection management ----------------------------------------------------------------

get_new_request <- function(server, username) {
  conn <- new_bo_request_reference() # make a new empty request reference
  base_url <- paste0("https://", server, "/biprws")
  # set request headers
  conn$request <- httr2::request(base_url) %>%
    httr2::req_headers("Accept" = "application/json") %>%
    httr2::req_headers("Content-Type" = "application/json") %>%
    httr2::req_headers("Host" = server) %>%
    httr2::req_headers("User" = username) %>%
    httr2::req_options(ssl_verifypeer = 0)
  log_info("Created connection to", base_url)
  conn
}

#' Open a connection to a BO server. You can keep parameters in environment variables
#'
#' @param server Server to connect to "server:port" (optional)
#' Defaults BO_SERVER environment variable
#' @param username (optional) Defaults to "BO_USERNAME environment variable
#' @param password (optional)
#' @param save_password Save the password encrypted in the database.
#' Saving password is required for automatic reconnection on token expiration
#'
#' @return Connection reference
#' @examples
#' # open a connection to the server in environment variable "BO_SERVER" using user in "BO_USERNAME".
#' # and password in the database
#' conn <- open_bo_connection()
#' @export
open_bo_connection <- function(server = Sys.getenv("BO_SERVER"),
                               username = Sys.getenv("BO_USERNAME"),
                               password = get_user_password(username, server),
                               save_password = TRUE) {
  conn <- get_new_request(server, username)
  # search for valid token matching server and username
  if (get_cached_token(conn, server, username)) {
     return(conn)
  }
  if (get_new_token(conn, server, username, password)) {
    if (save_password) {
      set_user_password(username, server, password)
    }
    return(conn)
  }
  stop("open_bo_connection failed")
}

#' Title
#'
#' @param conn Connection reference to reconnect
#'
#' @return Connection reference
#' @noRd

reconnect_bo_connection <- function(conn) {
  username = conn$request$headers[['User']]
  server <- conn$request$headers[['Host']]
  password = get_user_password(username, server)
  logger::log_info(paste("Reconnecting to", server, "as", username, ";d have password", !is_null_or_empty(password), ";t reconnect_bo_connection line 208"))
  if (get_cached_token(conn, server, username)) {
    return(conn)
  }
  if (get_new_token(conn, server, username, password)) {
    return(conn)
  }
  stop("reconnect_bo_connection failed")
}

#' Close connection to the BO server
#' @description
#' Closes connection to the BO server.
#' @details
#' As connection tokens are cached an reused to prevent flooding the BO
#' server with processes, it's not mandatory to call this.
#' @param conn Connection reference to close
#'
#' @return Connection reference
#' @export
close_bo_connection <- function(conn) {
  request <- check_bo_connection(conn)
  request %<>% httr2::req_url_path_append("logoff") %>%
    httr2::req_method('POST')
  response <- httr2::req_perform(request)
  report_request_result(request, response, ";Disconnect", "logOffBOSession line 129")
  return_bo_response_content(response)
}

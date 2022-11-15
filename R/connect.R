#' @importFrom methods hasArg new
#' @importFrom methods hasArg new
#' @importFrom utils head tail timestamp
#' @importFrom stats filter
#' @importFrom magrittr %<>% %>%
#' @importFrom httr GET POST PUT DELETE add_headers upload_file content config
#' @importFrom httr2 request req_headers req_options
#' @importFrom httr2 req_body_json req_body_file req_body_raw req_body_multipart
#' @importFrom httr2 req_perform req_dry_run
#' @importFrom httr2 resp_status resp_body_json resp_body_xml resp_body_html resp_body_string
#' @importFrom jsonlite toJSON fromJSON
#' @include api-utils.R
#'
# prerequisites ----------------------------------------------------------------

# Create reference class for httr::request

structure(
  list(
    httr_request = add_headers(),
    httr2_request = httr2::request("https://")
  ),
  class = "myrequest"
)

setOldClass("request")
setOldClass("httr2_request")
setRefClass("request_reference_class", fields = list(request='request', request2='httr2_request'))
new_bo_request_reference <- setRefClass("request_reference_class", fields = list(request = "request", request2 = "httr2_request"))

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

#' Get user rights on the BO server
#'
#' @param conn connection reference
#'
#' @return Response content or NULL
#' @export
get_user_rights2 <- function(conn) {
  # a POST query of item SI_ID = 1 is the test of a token
  request <- conn$request2
  request$url = paste0(request$url, '/raylight/v1/session/rights')
  response <- req_perform(request)
  if (resp_status(response) == 200) {
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
#' @export
#' @noRd
check_bo_connection_state <- function(conn) {
  if (hasArg("conn") && !is.null(conn)) {
    rights <- get_user_rights(conn)
    return(!is_empty(rights))
  }
  return(FALSE)
}

clear_token_header <- function(conn) {
  conn$request$headers <- conn$request$headers[!names(conn$request$headers)%in% c("X-SAP-LogonToken")]
}

try_token <- function(conn, server, token) {
  log_message("Trying token from database", ";", token, ";;try_token line 68")
  result <- tryCatch({
      # this sets the token in the mutable connection
      conn$request$headers[["X-SAP-LogonToken"]] <- token
      if (USE_HTTR2) {
        conn$request2 <- conn$request2 %>% req_headers("X-SAP-LogonToken" = token)
      }
      check_bo_connection_state(conn = conn)
    }, error = function(cond) {
      FALSE
  })
  if (!result) {
    clear_token_header(conn)
  }
  return(result)
}

check_bo_connection <- function(conn) {
  if (rlang::is_empty(conn)) {
    log_message("Empty connection reference",";check_bo_connection 83", level = "ERROR")
  }
  if (class(conn) == "request_reference_class") {
    if (check_bo_connection_state(conn)) {
      return(conn$request)
    } else {
      # reopen the connection using conn$request$username and conn$request$user_password
      open_bo_connection(conn=conn)
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
  tokens <- get_saved_tokens(username, server)$token # get matching tokens
  if (!is.null(tokens) && length(tokens) > 0) {
    for (i in 1:length(tokens)) {
      token <- tokens[i]
      if (try_token(conn = conn, server=server, token = token)) {
        log_with_separator("Reusing token", separator='=', width=120)
        log_debug(paste0("{token}"))
        return(TRUE)
      } else {
        log_with_separator("Removing token", separator='-', width=120)
        remove_cached_token(token)
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
  ) %>% listToJSON()
  url <- paste0(conn$request$url, "/v1/logon/long") # append longon endpoint to url
  response <- POST(url = url, body = body, conn$request)
  report_request_result(conn$request, response, "New connection", "open_bo_connection line 146")
  token <- content(response)$logontoken
  if (!is.null(token) && str_length(token) > 0) {
    conn$request$headers[["X-SAP-LogonToken"]] <- token
    if (USE_HTTR2) {
        conn$request2 <- req_headers(conn$request2, "X-SAP-LogonToken" = token)
    }
    save_bo_token(username, server, token)
  } else {
    stop(paste("Logon to ", server, "as", username, "failed"))
  }
  log_with_separator("New token", separator='+', width=120)
  log_debug(paste0("{token}"))
  return(TRUE)
}

# connection management ----------------------------------------------------------------

get_new_request <- function(conn, server, username) {
  if (!hasArg("conn") || is.null(conn)) {
    conn <- new_bo_request_reference() # make a new empty request reference
  }
  # set request headers
  conn$request <-
    add_headers("Accept" = "application/json",
                "Content-Type" = "application/json",
                "Host" = server)
  # set base url
  base_url <- paste0("https://", server, "/biprws")
  conn$request$url <- base_url
  if (USE_HTTR2) {
      conn$request2 <- httr2::request(base_url) %>%
      req_headers("Accept" = "application/json") %>%
      req_headers("Content-Type" = "application/json") %>%
      req_headers("Host" = server) %>%
      req_options(ssl_verifypeer = 0)
  }
  log_info("Created connection to", base_url)
  conn
}

#' Open a connection to a BO server. You can keep parameters in environment variables
#'
#' @param server Server to connect to "server:port" (optional). Defaults BO_SERVER environment variable
#' @param username (optional). Defaults to "BO_USERNAME environment variable
#' @param password (optional). Use getPass() to enter value
#' @param conn Connection reference to reuse (optional)
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
  if (!is_empty(conn)) {
    if (check_bo_connection_state(conn)) {
      return(conn)
    }
  } else {
    conn <- get_new_request(conn, server, username)
  }
  httr::set_config(config(ssl_verifypeer = 0L)) # skip certificate checks
  # reset request
  if (is_empty(username)) {
    if (!is_null_or_empty(conn$request$username)) {
      username <- conn$request$username
    }
  }
  if (get_cached_token(conn, server, username)) {
    # search for valid token matching server and username
    return(conn)
  }
  from_keyring <- FALSE
  if (is_empty(password)) {
    if (!is_null_or_empty(conn$request$user_password)) {
      password = decrypt_string(conn$request$user_password)
    } else {
      from_keyring <- TRUE
      password <- get_keyring_secret(username)
    }
  }
  if (get_new_token(conn, server, username, password)) {
    conn$request$username <- username
    conn$request$user_password <- encrypt_string(password)
    # if password is not from the keyring check keyring for existing password
    if (!from_keyring) {
      stored_password <- get_keyring_secret(username)
      if (!is.null(stored_password) && stored_password != password) {
        log_message("Updating keyring password for ", username, ";open_bo_connection line 208")
        set_keyring_secret(username, password)
      }
    }
    return(conn)
  }
  stop("open_bo_connection failed")
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
  url <- paste0(conn$request$url, "/logoff")
  response <- POST(url = url, request = request)
  report_request_result(request, response, ";Disconnect", "logOffBOSession line 129")
  conn
}

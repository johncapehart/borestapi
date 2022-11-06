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

remove_cached_token <- function(token) {
  mycat("removing token", token, ";remove_cached_token line 316")
  conn <- dbConnect(RSQLite::SQLite(), dbname = get_bo_token_database_path())
  dbExecute(conn, paste0("DELETE FROM tokens WHERE token='", token, "'"))
  mycat("token removed", token, "count", nrow(dbReadTable(conn, "tokens")), ";remove_cached_token line 335")
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


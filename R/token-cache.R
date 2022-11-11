#' @importFrom RSQLite dbConnect dbDisconnect dbExecute dbListTables dbReadTable dbWriteTable dbAppendTable

# token caching ----------------------------------------------------------------

get_bo_token_database_path <- function() {
  return(file.path(get_home_path(), "/bo_connections.sqlite"))
}

#' Clear all saved tokens
#'
#' @export
#' @noRd
clear_saved_tokens <- function() {
  db_conn <- dbConnect(RSQLite::SQLite(), dbname = get_bo_token_database_path())
  dbExecute(db_conn, paste0("DELETE FROM tokens"))
  log_message("Cleared saved tokens")
  dbDisconnect(db_conn)
}

get_token_count <- function() {
  db_conn <- dbConnect(RSQLite::SQLite(), dbname = get_bo_token_database_path())
  table <- dbListTables(db_conn) %>% keep(~ .x == "tokens")
  if ((length(table) > 0)) {
    tokens <- dbReadTable(db_conn, "tokens")
    dbDisconnect(db_conn)
    return(nrow(tokens))
  } else {
    dbDisconnect(db_conn)
  }
  return(0)
}

get_saved_tokens <- function(username, server) {
  db_conn <- dbConnect(RSQLite::SQLite(), dbname = get_bo_token_database_path())
  table <- dbListTables(db_conn) %>% keep(~ .x == "tokens")
  if (!(length(table) > 0)) {
    tokens <- tibble(timestamp = Date(0), usernamekey = character(0), serverkey = character(0), token = character(0))
    dbWriteTable(db_conn, "tokens", tokens, append = TRUE)
  } else {
    tokens <- dbReadTable(db_conn, "tokens")
  }
  dbDisconnect(db_conn)
  filteredtokens <- tokens %>%
    dplyr::filter(serverkey == server) %>%
    dplyr::filter(usernamekey == username) %>%
    dplyr::arrange(timestamp)
  log_message("get_saved_tokens token count", nrow(filteredtokens), "get_saved_tokens line 328")
  return(filteredtokens)
}

remove_cached_token <- function(token) {
  log_message("removing token", token, ";remove_cached_token line 316")
  db_conn <- dbConnect(RSQLite::SQLite(), dbname = get_bo_token_database_path())
  dbExecute(db_conn, paste0("DELETE FROM tokens WHERE token='", token, "'"))
  log_message("token removed", token, "count", nrow(dbReadTable(db_conn, "tokens")), ";remove_cached_token line 335")
  dbDisconnect(db_conn)
}

save_bo_token <- function(username, server, token) {
  db_conn <- dbConnect(RSQLite::SQLite(), dbname = get_bo_token_database_path())
  tokens <- tibble(timestamp = now(), usernamekey = username, serverkey = server, token = token)
  table <- dbListTables(db_conn) %>% keep(~ .x == "tokens")
  if (length(table) > 0) {
    dbAppendTable(db_conn, "tokens", tokens)
  } else {
    dbWriteTable(db_conn, "tokens", tokens, append = TRUE)
  }
  log_message("token saved", token, "count", nrow(dbReadTable(db_conn, "tokens")), ";save_bo_token line 316")
  dbDisconnect(db_conn)
}


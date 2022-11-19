#' @importFrom RSQLite dbConnect dbDisconnect dbExecute dbListTables dbReadTable dbWriteTable dbAppendTable
#' @importFrom lubridate now
#'
#'
#
# encryption ----------------------------------------------------------------

init_rsa_keys <- function() {
  if (!file.exists("~/.ssh/id_rsa")) {
    logger::log_debug("Initializing RSA keys at '~/.ssh'", ';t init_rsa_keys line 11')
    key <- rsa_keygen()
    if (!dir.exists('~/.ssh')) {
      dir.create('~/.ssh')
    }
    write_pem(key, "~/.ssh/id_rsa")
    write_ssh(key$pubkey, "~/.ssh/id_rsa.pub")
  }
}

encrypt_object <- function(plain) {
  init_rsa_keys()
  plain <- serialize(plain, NULL)
  out <- openssl::encrypt_envelope(plain)
  return(serialize(out, NULL))
}

decrypt_object <- function(cipher) {
  cipher <- unserialize(cipher)
  out <- openssl::decrypt_envelope(cipher$data, cipher$iv, cipher$session, key = openssl::my_key(), password = NULL)
  return(unserialize(out))
}

# database operations  ----------------------------------------------------------------

get_database_path <- function() {
  parent_dir <- file.path(get_home_path(), '.config/borestapi')
  if (!dir.exists(parent_dir)) {
    dir.create(parent_dir)
  }
  return(file.path(parent_dir, "bo_connections.sqlite"))
}

open_database <- function() {
  dbConnect(RSQLite::SQLite(), dbname = get_database_path())
}

close_database <- function(db_conn) {
  dbDisconnect(db_conn)
}

get_table <- function(db_conn, table_name = get_token_table_name()) {
  dbListTables(db_conn) %>% keep(~ .x == table_name)
}

get_token_table_name <- function() {
  'tokens'
}

#' Clear a table
#'
#' @param table_name
#'
#' @export
#' @noRd
clear_table <- function(table_name= get_token_table_name()) {
  db_conn <- open_database()
  rs <- RSQLite::dbSendStatement(db_conn, paste0("DELETE FROM ", table_name))
  RSQLite::dbClearResult(rs)
  log_debug("Cleared table", table_name, ";t clear_table line 57")
  close_database(db_conn)
}

get_table_row_count <- function(table_name= get_token_table_name()) {
  db_conn <- open_database()
  table <- get_table(db_conn, table_name)
  if ((length(table) > 0)) {
    tokens <- dbReadTable(db_conn, table_name)
    result <- nrow(tokens)
  } else {
    result <- 0
  }
  dbDisconnect(db_conn)
  return(result)
}

# Item operations ----------------------------------------------------------------

get_saved_items <- function(username, server, table_name = get_token_table_name()) {
  db_conn <- open_database()
  table <- get_table(db_conn, table_name)
  items <- NULL
  if ((length(table) > 0)) {
      items <- dbReadTable(db_conn, table_name)
      items %<>% dplyr::filter(serverkey == server) %>%
        dplyr::filter(usernamekey == username) %>%
        dplyr::arrange(timestamp)
      log_debug("get_saved_items item count", nrow(items), ";t get_saved_tokens line 328")
      if (nrow(items) > 0) {
        items %<>% mutate(value = decrypt_object(base64enc::base64decode(value)))
      }
  } else {
    items <- tibble(timestamp = double(0), usernamekey = character(0), serverkey = character(0), value = character(0))
  }
  close_database(db_conn)
  return(items)
}

remove_item <- function(username, server, table_name = get_token_table_name()) {
  log_debug("removing item", username, server, table_name, ";remove_item line 316")
  db_conn <- open_database()
  table <- get_table(db_conn, table_name)
  if (length(table) > 0) {
    rs <- RSQLite::dbSendStatement(db_conn, paste0("DELETE FROM ", table_name, " WHERE usernamekey='", username, "' AND serverkey='", server, "'"))
    RSQLite::dbClearResult(rs)
    log_debug("Item removed", username, server, "remaining count", nrow(dbReadTable(db_conn, table_name)), ";remove_item line 100")
  }
  close_database(db_conn)
}

save_item <- function(username, server, value, table_name = get_token_table_name()) {
  db_conn <- open_database()
  value <- base64enc::base64encode(encrypt_object(value))
  new_values <- tibble(timestamp = now(), usernamekey = username, serverkey = server, value)
  table <- get_table(db_conn, table_name)
  if (length(table) > 0) {
    dbAppendTable(db_conn, table_name, new_values)
  } else {
    dbWriteTable(db_conn, table_name, new_values, append = TRUE)
  }
  log_debug("item saved", username, server, "count", nrow(dbReadTable(db_conn, table_name)), ";save_item line 316")
  close_database(db_conn)
}


# Password operations

get_password_table_name <- function() {
  'credentials'
}

get_user_password <- function(username = Sys.getenv('BO_USERNAME'), server = Sys.getenv('BO_SERVER')) {
  get_saved_items(username, server, table_name = get_password_table_name())$value
}

set_user_password <- function(username = Sys.getenv('BO_USERNAME'), server = Sys.getenv('BO_SERVER'), password) {
  clear_user_password(username, server)
  save_item(username, server, table_name = get_password_table_name(), value = password)
}

clear_user_password <- function(username = Sys.getenv('BO_USERNAME'), server = Sys.getenv('BO_SERVER')) {
  remove_item(username, server, table_name = get_password_table_name())
}

clear_all_user_passwords <- function() {
  clear_table(table_name = get_password_table_name())
}


#' @importFrom magrittr %<>% %>%
#' @importFrom RSQLite dbConnect dbDisconnect dbExecute dbListTables dbReadTable dbWriteTable dbAppendTable

get_home_path <- function() {
  profile <- Sys.getenv('USERPROFILE')
  if (!is_null_or_empty(profile)) {
    home_path <- profile
  } else {
    home_path <- path.expand('~')
  }
  return(home_path)
}

# encryption ----------------------------------------------------------------

get_salt <- function(x) {
  serialize(openssl::sha256(serialize(x, NULL)), NULL)
}

init_rsa_keys <- function() {
  ssh_path <- file.path(get_home_path(), '.ssh')
  if (!dir.exists(ssh_path)) {
    dir.create(ssh_path)
  }
  if (!file.exists(file.path(ssh_path, "id_rsa"))) {
    logger::log_debug("Initializing RSA keys at ssh_path", ';t init_rsa_keys line 11')
    key <- openssl::rsa_keygen()
    openssl::write_pem(key, "~/.ssh/id_rsa")
    openssl::write_ssh(key$pubkey, "~/.ssh/id_rsa.pub")
  }
}

encrypt_object <- function(plain) {
  init_rsa_keys()
  plain <- c(serialize(plain, NULL), get_salt(plain))
  out <- openssl::encrypt_envelope(plain)
  return(serialize(out, NULL))
}

decrypt_object <- function(cipher) {
  cipher <- unserialize(cipher)
  out <- openssl::decrypt_envelope(cipher$data, cipher$iv, cipher$session, key = openssl::my_key(), password = NULL)
  out <- head(out, -122)
  return(unserialize(out))
}

# database operations  ----------------------------------------------------------------

get_database_path <- function() {
  config_dir <- file.path(get_home_path(), '.config')
  if (!dir.exists(config_dir)) {
    dir.create(config_dir)
  }
  parent_dir <- file.path(config_dir, 'borestapi')
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

get_table <- function(db_conn, table_name) {
  dbListTables(db_conn) %>% purrr::keep(~ .x == table_name)
}

get_token_table_name <- function() {
  'tokens'
}

#' Clear a table
#'
#' @param table_name
#'
#' @noRd
clear_table <- function(table_name= get_token_table_name()) {
  db_conn <- open_database()
  rs <- RSQLite::dbSendStatement(db_conn, paste0("DELETE FROM ", table_name))
  RSQLite::dbClearResult(rs)
  log_debug(paste("Cleared table", table_name, ";t clear_table line 57"))
  close_database(db_conn)
}

#' Get the number of rows in a table
#'
#' @param table_name
#'
#' @noRd
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

empty_row <- function() {
  tibble::tibble(timestamp = double(0), usernamekey = character(0), serverkey = character(0), value = character(0))
}

#' Get items from table
#'
#' @param table_name Name of the table in the database
#' @param username Username key
#' @param server Server key
#'
#' @noRd
get_saved_items <- function(username = Sys.getenv('BO_USERNAME'), server = Sys.getenv('BO_SERVER'), table_name) {
  db_conn <- open_database()
  table <- get_table(db_conn, table_name)
  items <- NULL
  if ((length(table) > 0)) {
      items <- dbReadTable(db_conn, table_name)
      items %<>% dplyr::filter(.data$serverkey == server) %>%
        dplyr::filter(.data$usernamekey == username) %>%
        dplyr::arrange(.data$timestamp)
      log_debug(paste("get_saved_items table", table_name, "item count", nrow(items), ";t get_saved_items line 328"))
      if (nrow(items) > 0) {
        items %<>% dplyr::mutate(value = decrypt_object(base64enc::base64decode(.data$value)))
      }
  } else {
    items <- empty_row()
  }
  close_database(db_conn)
  return(items)
}

#' Remove an item from table
#'
#' @param table_name Name of the table c('tokens','credentials')
#' @param username  Name of the user
#' @param server Name of the server
#'
#' @noRd
remove_item <- function(username = Sys.getenv('BO_USERNAME'), server = Sys.getenv('BO_SERVER'), table_name) {
  log_debug(paste("Removing item", username, server, table_name, ";tremove_item line 316"))
  db_conn <- open_database()
  table <- get_table(db_conn, table_name)
  if (length(table) > 0) {
    rs <- RSQLite::dbSendStatement(db_conn, paste0("DELETE FROM ", table_name, " WHERE usernamekey='", username, "' AND serverkey='", server, "'"))
    RSQLite::dbClearResult(rs)
    log_debug("Item removed", username, server, "remaining count", nrow(dbReadTable(db_conn, table_name)), ";remove_item line 100")
  }
  close_database(db_conn)
}

#' Save an item from table
#'
#' @param table_name Name of the table c('tokens','credentials')
#' @param username  Name of the user
#' @param server Name of the server
#' @param value Value for item
#'
#' @noRd
save_item <- function(username = Sys.getenv('BO_USERNAME'), server = Sys.getenv('BO_SERVER'), value, table_name) {
  db_conn <- open_database()
  value <- base64enc::base64encode(encrypt_object(value))
  new_values <- tibble::tibble(timestamp = lubridate::now(), usernamekey = username, serverkey = server, value = value)
  table <- get_table(db_conn, table_name)
  if (length(table) > 0) {
    dbAppendTable(db_conn, table_name, new_values)
  } else {
    dbWriteTable(db_conn, table_name, new_values, append = TRUE)
  }
  log_debug("item saved", username, server, "count", nrow(dbReadTable(db_conn, table_name)), ";save_item line 316")
  close_database(db_conn)
}

# Token operations ----------------------------------------------------------------

#' Get tokens
#'
#' @param username Name of the user
#' @param server Name of the server (host) with port
#'
#' @return Items matching keys
#' @export
get_tokens <- function(username, server) {
  get_saved_items(username, server, get_token_table_name())
}

#' Get tokens
#'
#' @param username Name of the user
#' @param server Name of the server (host) with port
#' @param token token to save
#'
#' @return NULL
#' @export
save_token <- function(username, server, token) {
  if (stringr::str_detect(token, 'mock')) {
    log_warn(paste("Skipping saving mock token for", username, server))
  }
  save_item(username, server, token, table_name = get_token_table_name())
}

#' Remove tokens
#'
#' @param username Name of the user
#' @param server Name of the server (host) with port
#'
#' @return NULL
#' @export
remove_token <- function(username, server) {
  remove_item(username, server, table_name = get_token_table_name())
}

#' Get count of saved tokens
#'
#' @return NULL
#' @export
get_token_count <- function() {
  get_table_row_count(table_name = get_token_table_name())
}

#' Clear all tokens
#'
#' @return NULL
#' @export
clear_all_tokens <- function() {
  clear_table(table_name = get_token_table_name())
}

# Password operations ----------------------------------------------------------------

get_password_table_name <- function() {
  'credentials'
}

#' Title
#'
#' @param username Name of the user
#' @param server Name of the server (host) with port
#'
#' @return Password
#' @export
get_user_password <- function(username = Sys.getenv('BO_USERNAME'), server = Sys.getenv('BO_SERVER')) {
  get_saved_items(username, server, table_name = get_password_table_name())$value
}

#' Set the password for a user in the connections database
#' @param username Name of the user
#' @param server Name of the server (host) with port
#' @param password Value for password
#'
#' @return NULL
#' @export
set_user_password <- function(username = Sys.getenv('BO_USERNAME'), server = Sys.getenv('BO_SERVER'), password) {
  clear_user_password(username, server)
  save_item(username, server, table_name = get_password_table_name(), value = password)
}

#' Clear the password for a user in the connections database
#' @param username Name of the user
#' @param server Name of the server (host) with port
#'
#' @return NULL
#' @export
clear_user_password <- function(username = Sys.getenv('BO_USERNAME'), server = Sys.getenv('BO_SERVER')) {
  remove_item(username, server, table_name = get_password_table_name())
}

#' Clear all user passwords
#'
#' @return NULL
#' @export
clear_all_user_passwords <- function() {
  clear_table(table_name = get_password_table_name())
}


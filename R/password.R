# string encryption ----------------------------------------------------------------
#' @importFrom sodium keygen pubkey simple_encrypt simple_decrypt

get_encryption_seed <- function() {
  user = Sys.getenv('USER')
  seed = serialize(digest::digest(paste0('salt', user), 'sha1'), NULL)
  seed <- seed[1:32]
  return(seed)
}

global_encryption_key <- keygen(seed=get_encryption_seed())

get_encryption_key <- function() {
  return(global_encryption_key)
}

encrypt_string <- function(plaintext) {
  key <- get_encryption_key()
  pub <- pubkey(key)

  msg <- serialize(plaintext, NULL)
  # Encrypt message with pubkey
  ciphertext <- simple_encrypt(msg, pub)
  return(ciphertext)
}

decrypt_string <- function(ciphertext) {
  key <- get_encryption_key()
  out <- simple_decrypt(ciphertext, key)
  plaintext <- unserialize(out)
  return(plaintext)
}

# password storage and retrieval ----------------------------------------------------------------

# get_keyring_path <- function() {
#   home_path <- get_home_path()
#   keyring_path <- file.path(home_path, Sys.getenv("BO_KEYRING_FILE_PATH"))
#   return(keyring_path)
# }

get_keyring_file_password <- function() {
  user = Sys.getenv('USER')
  return(digest::digest(paste0('salt', user), 'sha1'))
}

# # clear_keyring <- function() {
# #   log_message(paste("Removing keyring at", get_keyring_path()), ";;clear_keyring line 41")
# #   file.remove(get_keyring_path())
# # }
# #
# create_keyring_file <- function(kr) {
#   log_message(paste(";Creating new keyring at", "create_keyring_file line 46"))
#   kr$keyring_create('login') #, password = get_keyring_file_password())
#   kr$kerying_set_default(kr)
#   return(kr)
# }

get_keyring <- function() {
  #options(keyring_backend = "file")
  kr <- keyring::default_backend()
  # keyring_path <- get_keyring_path()
  # if (!file.exists(keyring_path)) {
  #   log_message("get_keyring keyring file not found at", get_keyring_path(), "get_keyring ilne 56")
  #   kr <- create_keyring_file(kr)
  # }
  kr
}

unlock_keyring <- function() {
  kr <- get_keyring()
  haveKeyring <- nrow(kr$keyring_list() > 0)
  if (!haveKeyring) {
    kr$keyring_create(kr$keyring_default(), password=get_keyring_file_password())
  }
  tryCatch({
    if (kr$keyring_is_locked()) {
      log_message(";", "unlock_keyring keyring is locked;;unlock_keyring line 66")
      kr$keyring_unlock(password = get_keyring_file_password())
    }
  }, error=function(cond){
    log_message("Could not unlock keyring;unlock_keyring line 68")
    #clear_keyring()
    #kr <- get_keyring()
    #kr$keyring_unlock(password = get_keyring_file_password())
  })
  return(kr)
}

#' List all secrets from the keyring
#'
#' @param name Secret name
#'
#' @export
list_keyring <- function() {
  kr <- unlock_keyring()
  log_message(";list_keyring;; line 96")
  kr$get(service='borestapi')
}

#' Delete a secret from the keyring
#'
#' @param name Secret name
#'
#' @export
delete_keyring_secret <- function(name) {
  kr <- unlock_keyring()
  # username below is the keyring entry name
  kr$delete('borestapi', username = name)
  log_message("delete_keyring_secret", name, ";; line 108")
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
  log_message("set_keyring_secret", name, ";;set_keyring_secret line 120")
}

#' Get a secret from the keyring
#'
#' @param name Secret name
#'
#' @return Secret value
#' @export

get_keyring_secret <- function(name) {
  #keyring_path <- get_keyring_path()
  service_name <- 'borestapi'
  kr <- unlock_keyring()
  secret <- tryCatch(
    {
      log_message("get_keyring_secret", name, ";;get_keyring_secret line 137")
      kr$get(service_name, username = name)
    },
    error = function(cond) {
      NULL
    }
  )
  if (is.null(secret)) {
    if (interactive()) {
      # if interactive we can prompt for value
      prompt <- paste("Password for", name, ':')
      kr$set(service_name, username = name, prompt = prompt)
      secret <- kr$get(service_name, username = name)
    }
  }
  if (is.null(secret)) {
    log_message("No entry ", name, " for service", service_name, ";;get_keyring_secret line 129", level = 'WARN')
  } else {
    log_message("Retrieved keyring entry", name, ";secret length", str_length(secret) , ";;get_keyring_secret line 119")
  }
  return(secret)
}

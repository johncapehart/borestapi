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
#' @noRd
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

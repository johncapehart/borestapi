#' @importFrom futile.logger layout.simple layout.json flog.debug flog.error flog.info flog.layout flog.warn
#' @importFrom futile.logger appender.console flog.appender


isNullOrEmpty <- function(v) {
  if (!missing(v) && !is.null(v) && !is.na(v) && length(v) > 0) {
    return(!(str_length(v) > 0))
  }
  return(TRUE)
}

logger_ready <- FALSE

init_log <- function() {
  flog.layout(layout.json)
  flog.appender(appender.console(), name='console')
  flog.layout(layout.simple, name='console')
  logger_ready <- TRUE
}

logg <- function(loggername, level, s) {
  if (!logger_ready) init_log()
  switch(level,
         info = flog.info(msg=s, name = loggername),
         debug = flog.debug(msg=s, name = loggername),
         warning = flog.warn(msg=s, name = loggername),
         error = flog.error(msg=s, name = loggername),
         flog.info(msg=s, name = loggername)
  )
}

mycat <- function(..., file = stdout(), duration = 15, level = "info") {
  s <- paste(paste(unlist(list(...)), collapse = " "), collapse = " ")
  cat(s, '\n')
  return()
  tryCatch(
    {
      loggernames <- c('file','console','notification')
      result <- lapply(loggernames, function(x)logg(loggername=x,level = level, s = s))

      # cat(s, file = file)
      # if (exists("gsession", global_env())) {
      #   session <- get("gsession", global_env())
      #   if (!(level %in% c("default", "message", "warning", "error"))) {
      #     level <- "default"
      #   }
      #   s2 <- (str_split(s, ";") %>% unlist())[1]
      #   if (str_length(s2) > 0) {
      #     showNotification(substr(s2, 1, 120), type = level, duration = duration, session = session)
      #   }
      # }
    },
    error = function(cond) {
      tryCatch(
        {
          cat(paste("Attempted message", s), file=stderr())
          cat(paste("Logging error", "mycat 161", cond, level), file = stderr())
        }
      )
    }
  )
}


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
  futile.logger::flog.threshold('WARN')
  cat("log level",futile.logger::flog.threshold())
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


#' @importFrom futile.logger layout.simple layout.json
#' @importFrom futile.logger flog.trace flog.debug flog.info flog.warn flog.error
#' @importFrom futile.logger TRACE DEBUG INFO WARN ERROR
#' @importFrom futile.logger appender.console flog.appender flog.layout
#' @importFrom stringr str_to_upper

is_null_or_empty <- function(v) {
  if (!missing(v) && !is.null(v) && !is.na(v) && length(v) > 0) {
    return(!(str_length(v) > 0))
  }
  return(TRUE)
}

default_logging_level <- ifelse (!is_null_or_empty(Sys.getenv("BO_DEFAULT_LOGGING_LEVEL")), Sys.getenv("BO_DEFAULT_LOGGING_LEVEL"), "DEBUG")

set_logging_threshold <- function(threshold) {
  threshold <- switch(threshold,
                      'TRACE' = futile.logger::TRACE,
                      'DEBUG' = futile.logger::DEBUG,
                      'INFO' = futile.logger::INFO,
                      'WARN' = futile.logger::WARN,
                      'ERROR' = futile.logger::ERROR,
                      futile.logger::INFO
  )
  futile.logger::flog.threshold(threshold=threshold, name='ROOT')
  lapply(loggernames, function(x)futile.logger::flog.threshold(threshold, name=x))
}

get_logging_threshold <- function() {
  futile.logger::flog.threshold()
}

loggernames <- c('console') # c('console','file','notification')

init_log <- function() {
  flog.layout(layout.json)
  flog.appender(appender.console(), name='console')
  flog.layout(layout.simple, name='console')
  set_logging_threshold(Sys.getenv("BO_LOGGING_THRESHOLD"))
  logger_ready <<- TRUE
}

logg <- function(loggername, level, s) {
  if (!exists('logger_ready') || is_null(logger_ready) || !logger_ready) init_log()
  switch(level,
         'TRACE' = flog.trace(msg=s, name = loggername),
         'DEBUG' = flog.debug(msg=s, name = loggername),
         'INFO' = flog.info(msg=s, name = loggername),
         'WARN' = flog.warn(msg=s, name = loggername),
         'ERROR' = flog.error(msg=s, name = loggername),
         flog.info(msg=s, name = loggername)
  )
}

log_message <- function(..., trace, file = stdout(), duration = 15, level = default_logging_level) {
  level <- stringr::str_to_upper(level)
  s <- paste(paste(unlist(list(...)), collapse = " "), collapse = " ")
  threshold <- get_logging_threshold()
  # remove trailing part of message if level is DEBUG or INFO
  s <- switch(threshold,
              'TRACE' = s,
              'DEBUG' = stringr::str_remove(s,';;.+$'),
              stringr::str_remove(s,';.+$')
  )
  #cat(s, '\n')
  tryCatch(
    {
      result <- lapply(loggernames, function(x)logg(loggername=x,level = level, s = s))
    },
    error = function(cond) {
      tryCatch(
        {
          cat(paste("Attempted message", s), file=stderr())
          cat(paste("Logging error", "log_message 161", cond, level), file = stderr())
        }
      )
    }
  )
}


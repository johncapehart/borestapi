#' @importFrom futile.logger layout.simple layout.json
#' @importFrom futile.logger flog.trace flog.debug flog.info flog.warn flog.error
#' @importFrom futile.logger appender.console flog.appender flog.layout
#' @importFrom stringr str_to_upper str_flatten
#'
#' @importFrom logger log_debug log_error log_errors log_fatal log_info log_warn log_trace log_with_separator
#' @importFrom logger layout_simple layout_glue_colors layout_json layout_glue_generator
#' @importFrom logger log_threshold DEBUG ERROR FATAL INFO WARN TRACE
#'
#' @importFrom crayon combine_styles make_style reset
#'
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
                      'FATAL' = futile.logger::FATAL,
                      futile.logger::INFO
  )
  threshold2 <- switch(threshold,
                      'TRACE' = logger::TRACE,
                      'DEBUG' = logger::DEBUG,
                      'INFO' = logger::INFO,
                      'WARN' = logger::WARN,
                      'ERROR' = logger::ERROR,
                      'FATAL' = logger::FATAL,
                      logger::INFO
  )
  futile.logger::flog.threshold(threshold=threshold, name='ROOT')
  logger::log_threshold(threshold2)
  lapply(loggernames, function(x)futile.logger::flog.threshold(threshold, name=x))
}

get_logging_threshold <- function() {
  futile.logger::flog.threshold()
}

loggernames <- c('console') # c('console','file','notification')

#' Initializes the logger (called from .onLoad)
#'
#' @return nothing
#' @export


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

#' Title
#'
#' @param msg The message to log
#' @param level The logging level
#'
#' @return nothing
#' @export

my_log_colorize <- function(msg, level) {
  color <- switch(
    attr(level, 'level'),
    'FATAL'   = crayon::combine_styles(crayon::bold, crayon::make_style('red1')),
    'ERROR'   = crayon::make_style('red4'),
    'WARN'    = crayon::make_style('darkorange'),
    'SUCCESS' = crayon::combine_styles(crayon::bold, crayon::make_style('green4')),
    'INFO'    = crayon::make_style('gray40'),
    'DEBUG'   = crayon::make_style('deepskyblue4'),
    'TRACE'   = crayon::make_style('dodgerblue4'),
    crayon::combine_styles(crayon::strikethrough, crayon::make_style('gry100'))
  )
  paste0(color(msg),reset(''))
}

#' Title
#'
#' @param msg The message to log
#' @param level The logging level
#'
#' @return nothing
#' @export
#'
split_message<-function(msg, level) {
  smsg <- str_split(paste0('x', msg), ';(?=[\\w])')[[1]]
  smsg2 <- lapply(smsg,  function(s) {
    tag = substring(s,1,1)
    s <- trimws(substring(s, 2)) # remove first character
    s2 <- switch(tag,
                 'i'= ifelse (log_threshold()>=logger::INFO,my_log_colorize(s, INFO),''),
                 'd'= ifelse (log_threshold()>=logger::DEBUG,my_log_colorize(s, DEBUG),''),
                 't'= ifelse (log_threshold()>=logger::TRACE,my_log_colorize(s, TRACE),''),
                 my_log_colorize(s, level)
    )
    s2
  })
  msg3 <- stringr::str_flatten(smsg2, collapse=' ')
  msg3
}

init_log <- function() {
  flog.layout(layout.json)
  flog.appender(appender.console(), name='console')
  flog.layout(layout.simple, name='console')
  set_logging_threshold(Sys.getenv("BO_LOGGING_THRESHOLD"))
  logger_ready <<- TRUE

  console_layout <- logger::layout_glue_generator(
    format = paste(
      '{crayon::bold(my_log_colorize(level, levelr))}',
      '{crayon::make_style("gray40")(format(time, "[%Y-%m-%d %H:%M:%S]")
    )}',
      '{split_message(msg, levelr)}'))

  logger::log_layout(console_layout)
  threshold0 <- Sys.getenv("BO_LOGGING_THRESHOLDxxx")
  if (is_null_or_empty(threshold0)) {
    threshold0 <- logger::INFO
  }
  logger::log_threshold(threshold0)
}





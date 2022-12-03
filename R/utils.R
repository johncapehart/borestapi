#' @importFrom logger log_debug log_error log_errors log_fatal log_info log_warn log_trace log_with_separator
#' @importFrom logger layout_simple layout_glue_colors layout_json layout_glue_generator
#' @importFrom logger log_threshold DEBUG ERROR FATAL INFO WARN TRACE
#'
#' @importFrom crayon combine_styles make_style reset
#'
is_null_or_empty <- function(v) {
  if (!missing(v) && !is.null(v) && !is.na(v) && length(v) > 0) {
    return(!(stringr::str_length(v) > 0))
  }
  return(TRUE)
}

default_logging_level <- ifelse (!is_null_or_empty(Sys.getenv("BO_DEFAULT_LOGGING_LEVEL")), Sys.getenv("BO_DEFAULT_LOGGING_LEVEL"), "DEBUG")

set_logging_threshold <- function(threshold) {
  if (is.character(threshold)) {
    threshold <- switch(threshold,
                      'TRACE' = logger::TRACE,
                      'DEBUG' = logger::DEBUG,
                      'INFO' = logger::INFO,
                      'WARN' = logger::WARN,
                      'ERROR' = logger::ERROR,
                      'FATAL' = logger::FATAL,
                      logger::INFO
    )
    logger::log_threshold(threshold)
  }
}

get_logging_threshold <- function() {
  logger::log_threshold()
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

#' Colorize a string by embedded tags starting with ';'
#'
#' @param msg The message to log
#' @param level The logging level
#'
#' @return Colorized string
#' @export
#'
split_message<-function(msg, level) {
  smsg <- stringr::str_split(paste0('x', msg), ';(?=(I|D|T|i|d|t))')[[1]]
  smsg2 <- lapply(smsg,  function(s) {
    tag = stringr::str_to_lower(substring(s,1,1))
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

# https://www.r-bloggers.com/2013/04/package-wide-variablescache-in-r-packages/
cacheEnv <- base::new.env()

init_log <- function() {
  if (!exists('logger_ready', envir=cacheEnv) || !get('logger_ready', envir=cacheEnv)) {
    console_layout <- logger::layout_glue_generator(
    format = paste(
      '{crayon::bold(my_log_colorize(level, levelr))}',
      '{crayon::make_style("gray40")(format(time, "[%Y-%m-%d %H:%M:%S]")
    )}',
      '{split_message(msg, levelr)}'))

    logger::log_layout(console_layout)
    threshold0 <- Sys.getenv("BO_LOGGING_THRESHOLD")
    if (is_null_or_empty(threshold0)) {
      threshold0 <- logger::INFO
    }
    logger::log_threshold(threshold0)
    assign('logger_ready', TRUE, envir=cacheEnv)
  }
}

#' Test if running under httptest2
#'
#' @return Boolean TRUE if running under httptest2
#'
#' @noRd
in_httptest2 <- function() {
  call <- head(sys.calls(), -1) %>% purrr::keep(~ stringr::str_detect(as.character(.x)[[1]], 'with_mock_dir'))
  # print(call)
  return(!is.null(call) && length(call) > 0)
}





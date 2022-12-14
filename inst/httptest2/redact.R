function (request_or_response) {
  require(magrittr, quietly=TRUE)
  # redact json field from response body
  redact_json_field <- function(request_or_response, field) {
    pattern <- paste0('"', field, '":"[^"]*"')
    redaction <- paste0('"', field, '":"REDACTED"')
    request_or_response %>% gsub_response(pattern, redaction, ignore.case = TRUE)
  }
  # redact list field from request body
  redact_list_field <- function(request_or_response, field) {
    if(is.list(request_or_response$body) && is.list(request_or_response$body$data)) {
      if(!is.null(request_or_response$body$data[[field]])) {
        request_or_response$body$data[[field]] <- 'REDACTED'
      }
    }
    request_or_response
  }
  # browser()
  is_request <- length(as.list(request_or_response)) == 7
  result <- request_or_response %>%
    redact_headers("X-SAP-LogonToken") %>%
    redact_headers("Host") %>%
    redact_headers("User") %>%
    gsub_response("https\\://.+/biprws/", "") %>% # remove beginning of url to shorten paths
    redact_json_field('logontoken') %>%
    redact_json_field('SI_OWNER') %>%
    redact_json_field('SI_NAME') %>%
    redact_json_field('ownerid') %>%
    redact_list_field('username') %>%
    redact_list_field('password')

  result %<>% gsub_response(Sys.getenv('BO_TEST_USERNAME'), 'REDACTED', ignore.case = TRUE) %>% # remove test user name
    gsub_response(stringr::str_replace(Sys.getenv('BO_TEST_SERVER'), ':.+$', ''), 'REDACTED', ignore.case = TRUE) %>% # remove test BO server name
    gsub_response(Sys.info()['nodename'], 'REDACTED', ignore.case = TRUE) %>% # remove host name
    gsub_response(Sys.getenv('BO_TEST_REDACTION_LIST'), 'redacted', ignore.case = TRUE) %>% # remove based on match in environment variable
    gsub_response('(redactedredacted|redacted.redacted)', 'redacted', ignore.case = TRUE) # remove multiple

  # url <- httptest2::build_mock_url(result)
  log_warn(paste("Redacting request", request_or_response$url))
  #browser()
  result
}

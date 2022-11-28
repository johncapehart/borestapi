function (request_or_response) {
  require(magrittr, quietly=TRUE)
  # redact json field from response body
  redact_json_field <- function(request_or_response, field) {
    pattern <- paste0('"', field, '":".+"')
    redaction <- paste0('"', field, '":"REDACTED"')
    request_or_response %>% gsub_response(pattern, redaction)
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
  is_request <- length(as.list(request_or_response)) == 7
  result <- request_or_response %>%
    redact_headers("X-SAP-LogonToken") %>%
    redact_headers("Host") %>%
    redact_headers("User") %>%
    gsub_response("https\\://.+/biprws", "") %>%
    redact_json_field('logontoken') %>%
    redact_json_field('SI_OWNER') %>%
    redact_json_field('SI_NAME') %>%
    redact_list_field('username') %>%
    redact_list_field('password')
  # url <- httptest2::build_mock_url(result)
  # log_debug(paste("Mock url", url, "from", request_or_response$url))
  result
}

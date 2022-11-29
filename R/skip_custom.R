skip_if_httptest2 <- function() {
  skip_on_cran()
  if (!exists('in_httptest2')) {
    stop("Missing in_httptest2")
  }
  ## browser()
  if (in_httptest2()) {
    log_trace('skip_if_httptest2 mocking')
    skip("Not run in httrtest2")
  } else {
    log_trace('skip_if_httptest2 not mocking')
  }
}

skip_if_httptest2 <- function() {
  skip_on_cran()
  skip_if(in_httptest2())
}

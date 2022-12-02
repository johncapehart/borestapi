init_options <- function() {
}

in_httptest2 <- function() {
  call <- head(sys.calls(), -1) %>% keep(~ stringr::str_detect(as.character(.x)[[1]], 'with_mock_dir'))
  # print(call)
  return(!is.null(call) && length(call) > 0)
}

upload_bo_mtcars <- function(conn, n = NULL, suffix = 1) {
  df <- mtcars
  if (is.null(n)) {
    n <- nrow(df)
  }
  # create a date column for date filtering test
  min = paste0(format(Sys.Date(), '%Y'), '2022-01-01')
  max = paste0(format(Sys.Date(), '%Y'), '2022-12-31')
  df$date = seq(as.Date('2022-01-01'), as.Date('2022-12-31'), by = "day")[0:nrow(df)]
  # create column for rownames
  df <- tibble::rownames_to_column(df, "model")
  # slice the set
  df <- head(df, n)
  filename <- paste(Sys.getenv('BO_TEST_EXCEL_FILE_NAME'), suffix)
  folder_id = Sys.getenv('BO_TEST_FOLDER_ID')
  upload_df(conn, df, filename, folder_id, 'Cars')
}

#' Generate httptest2 mock files
#'
#' @return Nothing

#' @example
#' \donttest{
#' generate_mocks()
#' }
#' @noRd
generate_mocks <- function() {
  library(httptest2)
  devtools::load_all()
  withr::local_dir(file.path(Sys.getenv('RENV_PROJECT'), 'tests/testthat'))
  logger::log_threshold(WARN)
  # remove mock directory
  unlink('mock', recursive = TRUE)
  with_mock_dir('mock',{
    clear_all_tokens()# force new token
    local_options('bo_no_new_tokens'=FALSE) #nale new token
    conn<-open_bo_connection() # force a login with a new token
    local_options('bo_no_new_tokens'=TRUE) # block new tokens
    # httptest2::block_requests()
    devtools::test()
    # testthat::test_file('test-connect.R')
    # testthat::test_file('test-api-utils.R')
    # testthat::test_file('test-document.R')
    # testthat::test_file('test-report.R')
    # testthat::test_file('test-controls.R')
    # testthat::test_file('test-spreadsheet.R')
  })
}

#' Generate test_down report
#'
#' @return

#' @example
#' \donttest{
#' generate_test_report()
#' }
#' @noRd
generate_test_report <- function() {
  library(httptest2)
  library(testdown)
  devtools::load_all()

  logger::log_threshold(WARN)
  testdown::test_down(devtools::test())
}


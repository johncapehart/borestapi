init_options <- function() {
}

upload_bo_mtcars <- function(conn, filename, parent_folder, n = NULL, suffix = 1) {
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
  upload_bo_dataframe(conn, df, filename, parent_folder, sheetname = 'Cars')
}

get_test_folder_id <- function(conn) {
  head(get_bo_item(conn, name=Sys.getenv('BO_TEST_FOLDER_NAME'), kind='Folder')$SI_ID)
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
  require(httptest2)
  devtools::load_all()
  withr::local_dir(file.path(Sys.getenv('RENV_PROJECT'), 'tests/testthat'))
  logger::log_threshold(WARN)
  # remove mock directory
  unlink('mock', recursive = TRUE)
  httptest2::with_mock_dir('mock',{
    clear_all_tokens()# force new token
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

test_mocks <- function() {
  httptest2::with_mock_dir('mock',{
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


init_options <- function() {
}

in_httptest2 <- function() {
  call <- head(sys.calls(), -1) %>% keep(~ stringr::str_detect(as.character(.x)[[1]], 'with_mock_dir'))
  # print(call)
  return(!is.null(call) && length(call) > 0)
}

upload_bo_mtcars <- function(conn, n = NULL) {
  require(openxlsx, quietly=TRUE)
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
  filename <- Sys.getenv('BO_TEST_EXCEL_FILE_NAME')
  folder_id = Sys.getenv('BO_TEST_FOLDER_ID')
  upload_df(conn, df, filename, folder_id, 'Cars')
}

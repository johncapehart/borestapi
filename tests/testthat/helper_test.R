
upload_bo_mtcars <- function(conn, n = NULL) {
  df <- mtcars
  filename <- Sys.getenv('BO_TEST_EXCEL_FILE_NAME')
  folder_id = Sys.getenv('BO_TEST_FOLDER_ID')
  library(openxlsx)
  # https://stackoverflow.com/questions/21502332/generating-random-dates
  rdate <- function(x, min = paste0(format(Sys.Date(), '%Y'), '-01-01'), max = paste0(format(Sys.Date(), '%Y'), '-12-31')) {
    dates <- sample(seq(as.Date(min), as.Date(max), by = "day"), x, replace = TRUE)
  }
  df$date = rdate(nrow(mtcars))
  df <- tibble::rownames_to_column(df, "model")
  if (!is_null(n)) {
    df <- head(df, n)
  }
  upload_df(conn, df, filename, folder_id, 'Cars')
}

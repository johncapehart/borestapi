test_that(paste("Cars dataframe uploads to BO"), {
  conn<-open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))

  df <- mtcars
  filename <- Sys.getenv('BO_TEST_EXCEL_FILE_NAME')
  folder_id = Sys.getenv('BO_TEST_FOLDER_ID')
  library(openxlsx)
  # https://stackoverflow.com/questions/21502332/generating-random-dates
  rdate <- function(x, min = paste0(format(Sys.Date(), '%Y'), '-01-01'), max = paste0(format(Sys.Date(), '%Y'), '-12-31')) {
    dates <- sample(seq(as.Date(min), as.Date(max), by = "day"), x, replace = TRUE)
  }
  upload  upload_bo_spreadsheet(conn, filename, parent_folder = folder_id)
  sheet2 <- get_bo_spreadsheet_details(conn, filename, parent_folder = folder_id)
  expect_equal(sheet2$sheets$sheet[[1]], "Cars")
  library(parsedate)
  u2 <- parsedate::parse_iso_8601(sheet2$updated)
  expect_gt(u2, u1)
  file.remove(filename)})

# test_that(paste("Delete test spreadsheet"), {
#   file <- 'test.xlsx'
#   conn<-open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
#   sheet2 <- get_bo_spreadsheet_details(conn, file, parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'))
#   delete_bo_spreadsheet(conn,filename = file  , parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'))
#   sheet2 <- get_bo_spreadsheet_details(conn, file, parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'))
# })

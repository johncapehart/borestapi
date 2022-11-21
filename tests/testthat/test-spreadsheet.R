test_that(paste("Cars dataframe uploads to BO"), {
  conn<-open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))

  df <- tibble(mtcars)
  df <- head(df, 11)
  filename <- Sys.getenv('BO_TEST_EXCEL_FILE_NAME')
  folder_id = Sys.getenv('BO_TEST_FOLDER_ID')
  library(openxlsx)
  wb2 <- createWorkbook(mtcars)
  sheet <- wb2 %>% addWorksheet('Cars')
  writeDataTable(wb2, sheet, df)
  saveWorkbook(wb2, filename, overwrite = TRUE)
  u1 <- lubridate::now()
  Sys.sleep(5)
  upload_bo_spreadsheet(conn, filename, parent_folder = folder_id)
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

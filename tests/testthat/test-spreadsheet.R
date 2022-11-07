test_that(paste("Cars dataframe uploads to BO"), {
  conn<-open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))

  df <- tibble(mtcars)
  df <- head(df, 11)
  file <- 'test.xlsx'
  library(openxlsx)
  wb2 <- createWorkbook(mtcars)
  sheet <- wb2 %>% addWorksheet('Cars')
  writeDataTable(wb2, sheet, df)
  saveWorkbook(wb2, file, overwrite = TRUE)

  upload_bo_spreadsheet(conn, file, file, parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'))
  sheet2 <- get_bo_spreadsheet(conn, file, parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'))
  expect_equal(sheet2$spreadsheet$sheets$sheet[[1]], "Cars")
  file.remove(file)
})

# test_that(paste("Delete test spreadsheet"), {
#   file <- 'test.xlsx'
#   conn<-open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
#   sheet2 <- get_bo_spreadsheet(conn, file, parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'))
#   delete_bo_spreadsheet(conn,filename = file  , parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'))
#   sheet2 <- get_bo_spreadsheet(conn, file, parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'))
# })

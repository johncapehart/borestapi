test_that(paste("Cars dataframe uploads to BO"), {
  conn<-open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  filename <- Sys.getenv('BO_TEST_EXCEL_FILE_NAME')
  folder_id <- Sys.getenv('BO_TEST_FOLDER_ID')
  sheet1 <- get_bo_spreadsheet_details(conn, filename, parent_folder = folder_id)
  upload_bo_mtcars(conn)
  sheet2 <- get_bo_spreadsheet_details(conn, filename, parent_folder = folder_id)
  expect_equal(sheet2$sheets$sheet[[1]], "Cars")
  expect_true(sheet2$updated>sheet1$updated)
})

test_that(paste("Delete test spreadsheet"), {
  skip("Server not configured for deletions")
  file <- 'test.xlsx'
  conn<-open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  sheet2 <- get_bo_spreadsheet_details(conn, file, parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'))
  delete_bo_spreadsheet(conn,filename = file  , parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'))
  sheet2 <- get_bo_spreadsheet_details(conn, file, parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'))
})

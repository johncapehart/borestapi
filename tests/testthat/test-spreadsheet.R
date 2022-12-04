test_that(paste("Spreadsheet", Sys.getenv('BO_TEST_EXCEL_FILE_NAME'), 'exists'), {
  skip_if_httptest2()
  conn<-open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  filename <- Sys.getenv('BO_TEST_EXCEL_FILE_NAME')
  folder_id <- get_test_folder_id(conn)
  sheet1 <- get_bo_spreadsheet_details(conn, filename, parent_folder = folder_id)
  expect_equal(ignore_attr = TRUE, sheet1$name, filename)
})

test_that(paste("Cars dataframe uploads to BO"), {
  conn<-open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  suffix = 1
  filename <- stringr::str_replace(Sys.getenv('BO_TEST_EXCEL_FILE_NAME'), '\\.', paste0(suffix, '.'))
  folder_id <- get_test_folder_id(conn)
  upload_bo_mtcars(conn, filename, folder_id)
  sheet1 <- get_bo_spreadsheet_details(conn, filename, parent_folder = folder_id)
  upload_bo_mtcars(conn, filename, parent_folder = folder_id, 10)
  Sys.sleep(1)
  sheet2 <- get_bo_spreadsheet_details(conn, filename, parent_folder = folder_id)
  expect_equal(ignore_attr = TRUE, sheet2$sheets$sheet[[1]], "Cars")
  if (!in_httptest2()) {
    expect_true(sheet2$updated>sheet1$updated)
  }
  # clean up
  upload_bo_mtcars(conn, filename, parent_folder = folder_id)
})

test_that(paste("Cars dataframe uploads to BO as .xls"), {
  conn<-open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  suffix = 1
  filename <- stringr::str_replace(Sys.getenv('BO_TEST_EXCEL_FILE_NAME'), '\\.', paste0(suffix, '.'))
  filename <- stringr::str_replace(filename, '.xlsx$', '.xls')
  folder_id <- get_test_folder_id(conn)
  upload_bo_mtcars(conn, filename, parent_folder = folder_id, 10)
  sheet2 <- get_bo_spreadsheet_details(conn, filename, parent_folder = folder_id)
  expect_equal(ignore_attr = TRUE, sheet2$sheets$sheet[[1]], "Cars")
})

test_that(paste("Cars dataframe uploads to BO as .csv"), {
  skip("Server not configured for agnostic document uploads")
  conn<-open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  folder_id <- get_test_folder_id(conn)
  suffix = 1
  filename <- stringr::str_replace(Sys.getenv('BO_TEST_EXCEL_FILE_NAME'), '\\.', paste0(suffix, '.'))
  filename <- stringr::str_replace(filename, '.xlsx$', '.csv')
  upload_bo_mtcars(conn, filename, parent_folder = folder_id, 10)
})

test_that(paste("Getting children of a folder works"), {
  conn<-open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  items <- get_bo_folder_children(conn, parent_folder = folder_id)
  expect_gt(nrow(items), 0)
})

test_that(paste("Delete test spreadsheet"), {
  skip("Server not configured for deletions")
  conn<-open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  folder_id <- get_test_folder_id(conn)
  suffix = 1
  filename <- stringr::str_replace(Sys.getenv('BO_TEST_EXCEL_FILE_NAME'), '\\.', paste0(suffix, '.'))
  conn<-open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  sheet1 <- get_bo_spreadsheet_details(conn, file, parent_folder = folder_id)
  delete_bo_spreadsheet(conn,filename = file  , parent_folder = get_test_folder_id(conn))
  sheet2 <- get_bo_spreadsheet_details(conn, file, parent_folder = folder_id)
  expect_equal(ignore_attr = TRUE, nrow(sheet2), 0)
})

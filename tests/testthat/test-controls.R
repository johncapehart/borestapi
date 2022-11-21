test_that(paste("Getting controls works"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  filename <- Sys.getenv('BO_TEST_EXCEL_FILE_NAME')
  folder_id = Sys.getenv('BO_TEST_FOLDER_ID')
  document <- get_bo_item(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'), owner = NULL, kind = "Webi")
  controls <- get_bo_control_details(conn, document)
  expect_gt(nrow(controls), 0)
})

test_that(paste("Getting controls selection set works"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  filename <- Sys.getenv('BO_TEST_EXCEL_FILE_NAME')
  folder_id = Sys.getenv('BO_TEST_FOLDER_ID')
  document <- get_bo_item(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'), owner = NULL, kind = "Webi")
  controls <- get_bo_control_details(conn, document)
  get_bo_control_selection_set(conn, document, controls$name[[1]])
  expect_gt(nrow(controls), 0)
})

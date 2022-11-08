test_that(paste("Item", Sys.getenv('BO_TEST_DOCUMENT_NAME'), "is found"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  document <- get_bo_item(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'), owner = NULL, kind = "Webi")
  expect_equal(nrow(document), 1)
  expect_equal(document$SI_NAME, Sys.getenv('BO_TEST_DOCUMENT_NAME'))
})

test_that(paste("Multiple items are found in a folder"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  documents <- get_bo_item(conn, parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'))
  expect_gt(nrow(documents), 1)
})

test_that(paste("Multiple items are found in a folder matching type 'Excel'"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  documents <- get_bo_item(conn, parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'), kind='Excel')
  expect_gt(nrow(documents), 1)
})

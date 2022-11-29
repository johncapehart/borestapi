test_that(paste("Item", Sys.getenv('BO_TEST_DOCUMENT_NAME'), "is found"), {
  # browser()
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  d1 <- get_bo_item(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'), owner = NULL, kind = "Webi")
  print (nrow(d1))
  # browser()
  expect_equal(ignore_attr = TRUE, nrow(d1), 1)
})

test_that(paste("Multiple items are found in a folder"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  d2 <- get_bo_item(conn, parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'))
  expect_gt(nrow(d2), 1)
})

test_that(paste("Multiple items are found in a folder matching type 'Excel'"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  d3 <- get_bo_item(conn, parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'), kind='Excel')
  expect_gt(nrow(d3), 1)
})

test_that(paste("get_bo_document_details returns document details for ", Sys.getenv('BO_TEST_DOCUMENT_NAME')), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  document <- get_bo_item(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'), owner = NULL, kind = "Webi")
  document2 <- get_bo_document_details(conn, document)
  expect_equal(document2$name, Sys.getenv('BO_TEST_DOCUMENT_NAME'))
})

test_that(paste("Document", Sys.getenv('BO_TEST_DOCUMENT_NAME'), "refreshes"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  document <- get_bo_item(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'), owner = NULL, kind = "Webi")
  d1 <- get_bo_document_details(conn, document)
  refresh_bo_document_data_provider(conn, document=document, data_provider = Sys.getenv('BO_TEST_DATA_SOURCE'))
  close_bo_document(conn, document, save= TRUE)
  d2 <- get_bo_document_details(conn, document)
  u1 <- lubridate::parse_date_time(d1$updated, '%m, %d %Y %I:%M %p')
  u2 <- lubridate::parse_date_time(d2$updated, '%m, %d %Y %I:%M %p')
  expect_gt(u2, u1)
})

test_that(paste("Document", Sys.getenv('BO_TEST_DOCUMENT_NAME'), "has data in provider", Sys.getenv('BO_TEST_DATA_SOURCE')), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  document_name <- Sys.getenv('BO_TEST_DOCUMENT_NAME')
  folder_id = Sys.getenv('BO_TEST_FOLDER_ID')
  document <- get_bo_item(conn, name = document_name, parent_folder = folder_id, owner = NULL, kind = "Webi")
  df <- get_bo_data_provider_data(conn, document, data_provider = Sys.getenv('BO_TEST_DATA_SOURCE'))
  expect_gt(nrow(df), 0)
  expect_gt(ncol(df), 0)
})

test_that(paste("get_bo_document_details returns document details for ", Sys.getenv('BO_TEST_DOCUMENT_NAME')), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  document <- get_bo_item(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'), owner = NULL, kind = "Webi")
  document2 <- get_bo_document_details(conn, document)
  expect_equal(document2$name, Sys.getenv('BO_TEST_DOCUMENT_NAME'))
})


test_that(paste("Document", Sys.getenv('BO_TEST_DOCUMENT_NAME'), "has data in provider", Sys.getenv('BO_TEST_DATA_SOURCE')), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  document_name <- Sys.getenv('BO_TEST_DOCUMENT_NAME')
  folder_id = Sys.getenv('BO_TEST_FOLDER_ID')
  document <- get_bo_item(conn, name = document_name, parent_folder = folder_id, owner = NULL, kind = "Webi")
  dp1 <- get_bo_data_provider_details(conn, document,  Sys.getenv('BO_TEST_DATA_SOURCE_ID'))
  df <- get_bo_data_provider_data(conn, document, data_provider =  dp1$id)
  expect_gt(nrow(df), 0)
  expect_gt(ncol(df), 0)
})

test_that(paste("Document", Sys.getenv('BO_TEST_DOCUMENT_NAME'), "refreshes"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  document <- get_bo_item(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'), owner = NULL, kind = "Webi")
  d1 <- get_bo_document_details(conn, document)
  dp1 <- get_bo_data_provider_details(conn, document,  Sys.getenv('BO_TEST_DATA_SOURCE_ID'))
  df1 <- get_bo_data_provider_data(conn, document, data_provider = dp1$id)
  upload_bo_mtcars(conn, 10)
  refresh_bo_document_data_provider(conn, document=document, data_provider = dp1$id)
  close_bo_document(conn, document, save= TRUE)
  d2 <- get_bo_document_details(conn, document)
  df2 <- get_bo_data_provider_data(conn, document, data_provider = dp1$id)
  expect_equal(nrow(df2), 10)
  upload_bo_mtcars(conn)
  refresh_bo_document_data_provider(conn, document=document, data_provider = dp1$id)
  close_bo_document(conn, document, save= TRUE)
  d3 <- get_bo_document_details(conn, document)
  df3 <- get_bo_data_provider_data(conn, document, data_provider = dp1$id)
  expect_equal(nrow(df3), 32)
  expect_gt(nrow(df3), nrow(df2))
  # updated propety has high latency so not accurate for test
  if (0) {
    u1 <- lubridate::parse_date_time(d1$updated, '%m, %d %Y %I:%M %p')
    u3 <- lubridate::parse_date_time(d3$updated, '%m, %d %Y %I:%M %p')
    logger::log_warn(paste('********************************', u1, u3))
    expect_gt(u3, u1)
  }
})

test_that(paste("Copy document works"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  document <- get_bo_item(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'), owner = NULL, kind = "Webi")
  d1 <- get_bo_document_details(conn, document)
  result <- copy_bo_document(conn, document, parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'), paste(d1$name,'Copy Test'))
  d2 <- get_bo_document_details(conn, result$success$id)
  expect_match(d2$name, paste(d1$name,'Copy'))
})

test_that(paste("Create document works"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  result <- create_bo_document(conn, paste(Sys.getenv('BO_TEST_DOCUMENT_NAME'), "Create Test", lubridate::now()), Sys.getenv('BO_TEST_FOLDER_ID'))
  expect_match(result$success$message, "success")
})

test_that(paste("Delete document works"), {
  skip("Server not configured for deletions")
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  document <- get_bo_item(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'), owner = NULL, kind = "Webi")
  d1 <- get_bo_document_details(conn, document)
  document_copy <- get_bo_item(conn, name = paste(d1$name,'Copy'), parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'), owner = NULL, kind = "Webi")
  d1 <- get_bo_document_details(conn, document_copy)
  delete_bo_document(conn, d1)
})


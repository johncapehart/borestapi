test_that(paste("get_bo_document_details returns document details for", Sys.getenv('BO_TEST_DOCUMENT_NAME')), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  document <- get_bo_item(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), parent_folder = get_test_folder_id(conn), owner = NULL, kind = "Webi")
  document2 <- get_bo_document_details(conn, document)
  expect_equal(ignore_attr = TRUE, document2$name, Sys.getenv('BO_TEST_DOCUMENT_NAME'))
})

test_that(paste("Document", Sys.getenv('BO_TEST_DOCUMENT_NAME'), "has provider", Sys.getenv('BO_TEST_DATA_SOURCE')), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  document_name <- Sys.getenv('BO_TEST_DOCUMENT_NAME')
  folder_id = get_test_folder_id(conn)
  document <- get_bo_item(conn, name = document_name, parent_folder = folder_id, owner = NULL, kind = "Webi")
  dp1 <- get_bo_data_provider_details(conn, document,  Sys.getenv('BO_TEST_DATA_SOURCE_ID'))
  expect_equal(ignore_attr = TRUE, dp1$id, Sys.getenv('BO_TEST_DATA_SOURCE_ID'))
})

test_that(paste("Document", Sys.getenv('BO_TEST_DOCUMENT_NAME'), "has data in provider", Sys.getenv('BO_TEST_DATA_SOURCE')), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  document_name <- Sys.getenv('BO_TEST_DOCUMENT_NAME')
  folder_id <- get_test_folder_id(conn)
  document <- get_bo_item(conn, name = document_name, parent_folder = folder_id, owner = NULL, kind = "Webi")
  dp1 <- get_bo_data_provider_details(conn, document,  Sys.getenv('BO_TEST_DATA_SOURCE_ID'))
  df <- get_bo_data_provider_data(conn, document, data_provider =  dp1$id)
  expect_gt(nrow(df), 0)
  expect_gt(ncol(df), 0)
})

test_that(paste("Document", Sys.getenv('BO_TEST_DOCUMENT_NAME'), "refreshes"), {
  skip_if_httptest2() # mocks do not update row counts
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  document <- get_bo_item(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), parent_folder = get_test_folder_id(conn), owner = NULL, kind = "Webi")
  filename = Sys.getenv('BO_TEST_EXCEL_FILE_NAME')
  folder_id <- get_test_folder_id(conn)
  d1 <- get_bo_document_details(conn, document)
  dp1 <- get_bo_data_provider_details(conn, document,  Sys.getenv('BO_TEST_DATA_SOURCE_ID'))
  df1 <- get_bo_data_provider_data(conn, document, data_provider = dp1$id)
  upload_bo_mtcars(conn, filename, parent_folder = folder_id, 10)
  refresh_bo_data_provider(conn, document=document, data_provider = dp1$id)
  close_bo_document(conn, document, save= TRUE)
  #Sys.sleep(1)
  d2 <- get_bo_document_details(conn, document)
  df2 <- get_bo_data_provider_data(conn, document, data_provider = dp1$id)
  expect_equal(ignore_attr = TRUE, nrow(df2), 10)
  upload_bo_mtcars(conn, filename, parent_folder = folder_id)
  refresh_bo_data_provider(conn, document=document, data_provider = dp1$id)
  close_bo_document(conn, document, save= TRUE)
  #Sys.sleep(1)
  d3 <- get_bo_document_details(conn, document)
  df3 <- get_bo_data_provider_data(conn, document, data_provider = dp1$id)
  expect_equal(ignore_attr = TRUE, nrow(df3), 32)
  expect_gt(nrow(df3), nrow(df2))
  # updated property has high latency so not accurate for test
  # u1 <- lubridate::parse_date_time(d1$updated, '%m, %d %Y %I:%M %p')
  # u3 <- lubridate::parse_date_time(d3$updated, '%m, %d %Y %I:%M %p')
  # _xpect_gt(u3, u1) # mangle name to remove from testdown report
})

test_that(paste("Copy document works"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  document <- get_bo_item(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), parent_folder = get_test_folder_id(conn), owner = NULL, kind = "Webi")
  name <- paste(Sys.getenv('BO_TEST_DOCUMENT_NAME'), "Copy Test")
  existing_document <- get_bo_item(conn, name, parent_folder = get_test_folder_id(conn))
  if (nrow(existing_document) == 0) {
    result <- copy_bo_document(conn, document, parent_folder = get_test_folder_id(conn), name)
    expect_true(result$success$id > 0)
  } else {
    skip('Copy already exists')
  }
})

test_that(paste("Create document works"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  name <- paste(Sys.getenv('BO_TEST_DOCUMENT_NAME'), "Create Test")
  existing_document <- get_bo_item(conn, name, parent_folder = get_test_folder_id(conn))
  if (nrow(existing_document) == 0) {
    result <- create_bo_document(conn, name, get_test_folder_id(conn))
    expect_true(result$success$id > 0)
  } else {
    skip('Document already exists')
  }
})

test_that(paste("Delete document works"), {
  skip_if_httptest2()
  skip("Server not configured for deletions")
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  name <- paste(Sys.getenv('BO_TEST_DOCUMENT_NAME'), "Copy Test")
  document <- get_bo_item(conn, name = name, parent_folder = get_test_folder_id(conn), owner = NULL, kind = "Webi")
  d1 <- get_bo_document_details(conn, document)
  delete_bo_document(conn, d1)
})


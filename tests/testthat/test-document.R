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
  df <- get_bo_data_provider_data(conn, document, data_provider = Sys.getenv('BO_TEST_DATA_SOURCE_ID'))
  expect_gt(nrow(df), 0)
  expect_gt(ncol(df), 0)
})

test_that(paste("Document", Sys.getenv('BO_TEST_DOCUMENT_NAME'), "refreshes"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  document <- get_bo_item(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'), owner = NULL, kind = "Webi")
  d1 <<- get_bo_document_details(conn, document)
  df1 <<- get_bo_data_provider_data(conn, document, data_provider = Sys.getenv('BO_TEST_DATA_SOURCE_ID'))
  upload_bo_mtcars(conn, 10)
  refresh_bo_document_data_provider(conn, document=document, data_provider = Sys.getenv('BO_TEST_DATA_SOURCE_ID'))
  close_bo_document(conn, document, save= TRUE)
  df2 <<- get_bo_data_provider_data(conn, document, data_provider = Sys.getenv('BO_TEST_DATA_SOURCE_ID'))
  upload_bo_mtcars(conn)
  refresh_bo_document_data_provider(conn, document=document, data_provider = Sys.getenv('BO_TEST_DATA_SOURCE_ID'))
  close_bo_document(conn, document, save= TRUE)
  df3 <<- get_bo_data_provider_data(conn, document, data_provider = Sys.getenv('BO_TEST_DATA_SOURCE_ID'))
  d3 <<- get_bo_document_details(conn, document)
  u1 <- lubridate::parse_date_time(d1$updated, '%m, %d %Y %I:%M %p')
  u3 <- lubridate::parse_date_time(d3$updated, '%m, %d %Y %I:%M %p')
  logger::log_warn(paste('********************************', u1, u3))
  # updated date field has high latency
  #expect_gt(u2, u3)
  expect_gt(nrow(df3), nrow(df2))
})


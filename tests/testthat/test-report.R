
test_that(paste("Report", Sys.getenv('BO_TEST_DOCUMENT_NAME'), "has reports"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  document <- get_bo_item(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), parent_folder = get_test_folder_id(conn), owner = NULL, kind = "Webi")
  items <- get_bo_report_details(conn, document)
  expect_gt(nrow(items) ,0)
})

test_that(paste("Report", Sys.getenv('BO_TEST_DOCUMENT_NAME'), "has reports"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  document <- get_bo_item(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), parent_folder = get_test_folder_id(conn), owner = NULL, kind = "Webi")
  items <- get_bo_report_details(conn, document, 'Cars')
  expect_equal(ignore_attr = TRUE, nrow(items), 1)
})

test_that(paste("Report", Sys.getenv("BO_TEST_REPORT_NAME"), "has data"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  document <- get_bo_item(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), parent_folder = get_test_folder_id(conn), owner = NULL, kind = "Webi")
  df <- get_bo_report_data(conn, document,  Sys.getenv("BO_TEST_REPORT_NAME"))
  expect_gt(nrow(df), 0)
})

test_that(paste("Report", Sys.getenv('BO_TEST_DOCUMENT_NAME'), "R/RInputs has formula"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  document <- get_bo_item(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), parent_folder = get_test_folder_id(conn), owner = NULL, kind = "Webi")
  inputs <- get_bo_report_inputs(conn, document, "reports/R/RInputs")
  expect_gt(stringr::str_length(inputs$Formula), 20)
})

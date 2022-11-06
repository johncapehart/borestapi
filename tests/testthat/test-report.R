
test_that(paste("Report", Sys.getenv('BO_TEST_DOCUMENT_NAME'), "R/RInputs has formula"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  document <- get_bo_item_from_name(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), folder_id = Sys.getenv('BO_TEST_FOLDER_ID'), owner = NULL, kind = "Webi")
  inputs <- get_bo_report_inputs(conn, document, "reports/R/RInputs")
  expect_gt(str_length(inputs$Formula),0)
})

test_that(paste("Report", Sys.getenv("BO_TEST_REPORT_NAME"), "has data"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  document <- get_bo_item_from_name(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), folder_id = Sys.getenv('BO_TEST_FOLDER_ID'), owner = NULL, kind = "Webi")
  df <- get_bo_document_report(conn, document,  Sys.getenv("BO_TEST_REPORT_NAME"))
  expect_gt(nrow(df), 0)
})

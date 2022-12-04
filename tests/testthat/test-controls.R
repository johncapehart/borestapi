test_that(paste("Getting document controls works"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  filename <- Sys.getenv('BO_TEST_EXCEL_FILE_NAME')
  folder_id = get_test_folder_id(conn)
  document <- get_bo_item(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), parent_folder = get_test_folder_id(conn), owner = NULL, kind = "Webi")
  controls <- get_bo_control_details(conn, document)
  expect_gt(nrow(controls), 0)
  expect_equal(ignore_attr = TRUE, controls$name[1], 'cyl')
})

test_that(paste("Getting report controls works"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  filename <- Sys.getenv('BO_TEST_EXCEL_FILE_NAME')
  folder_id = get_test_folder_id(conn)
  document <- get_bo_item(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), parent_folder = get_test_folder_id(conn), owner = NULL, kind = "Webi")
  controls <- get_bo_control_details(conn, document, Sys.getenv('BO_TEST_REPORT_NAME'))
  expect_gt(nrow(controls), 0)
  expect_equal(ignore_attr = TRUE, controls$name[1], 'gear')
})

test_that(paste("Getting document controls selection set works"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  filename <- Sys.getenv('BO_TEST_EXCEL_FILE_NAME')
  folder_id = get_test_folder_id(conn)
  document <- get_bo_item(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), parent_folder = get_test_folder_id(conn), owner = NULL, kind = "Webi")
  controls <- get_bo_control_details(conn, document)
  set <- get_bo_control_selection_set(conn, document, control = controls$name[[1]])
  expect_gt(length(set), 0)
})

test_that(paste("Getting report controls selection set works"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  filename <- Sys.getenv('BO_TEST_EXCEL_FILE_NAME')
  folder_id = get_test_folder_id(conn)
  document <- get_bo_item(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), parent_folder = get_test_folder_id(conn), owner = NULL, kind = "Webi")
  controls <- get_bo_control_details(conn, document, Sys.getenv('BO_TEST_REPORT_NAME'))
  set <- get_bo_control_selection_set(conn, document, Sys.getenv('BO_TEST_REPORT_NAME'), controls$name[[1]])
  expect_gt(length(set), 0)
})

test_that(paste("Getting document controls selected items"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  filename <- Sys.getenv('BO_TEST_EXCEL_FILE_NAME')
  folder_id = get_test_folder_id(conn)
  document <- get_bo_item(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), parent_folder = get_test_folder_id(conn), owner = NULL, kind = "Webi")
  controls <- get_bo_control_details(conn, document)
  selection <- get_bo_control_selection(conn, document, control=controls$name[[1]])
  expect_false(is_empty(selection))
})

test_that(paste("Getting report control selected items"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  filename <- Sys.getenv('BO_TEST_EXCEL_FILE_NAME')
  folder_id = get_test_folder_id(conn)
  reportname <- Sys.getenv('BO_TEST_REPORT_NAME')
  document <- get_bo_item(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), parent_folder = get_test_folder_id(conn), owner = NULL, kind = "Webi")
  controls <- get_bo_control_details(conn, document, reportname)
  selection <- get_bo_control_selection(conn, document, reportname, controls$name[[1]])
  expect_false(is_empty(selection))
})

test_that(paste("Setting document control selected items"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  filename <- Sys.getenv('BO_TEST_EXCEL_FILE_NAME')
  folder_id = get_test_folder_id(conn)
  document <- get_bo_item(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), parent_folder = get_test_folder_id(conn), owner = NULL, kind = "Webi")
  controls <- get_bo_control_details(conn, document)
  set_bo_control_selection(conn,document,control=controls$name[[1]],all=TRUE)
  close_bo_document(conn, document, save= TRUE)
  df1 <- get_bo_report_data(conn, document, Sys.getenv('BO_TEST_REPORT_NAME'))
  set_bo_control_selection(conn,document,control=controls$name[[1]],selections=c('6','8'))
  close_bo_document(conn, document, save= TRUE)
  df2 <- get_bo_report_data(conn, document, Sys.getenv('BO_TEST_REPORT_NAME'))
  expect_true(nrow(df2) > 0)
  if (!in_httptest2()) {
    expect_gt(nrow(df1), nrow(df2))
  }
})

test_that(paste("Setting report control selected items"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  filename <- Sys.getenv('BO_TEST_EXCEL_FILE_NAME')
  folder_id = get_test_folder_id(conn)
  reportname <- Sys.getenv('BO_TEST_REPORT_NAME')
  document <- get_bo_item(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), parent_folder = get_test_folder_id(conn), owner = NULL, kind = "Webi")
  controls <- get_bo_control_details(conn, document, reportname)
  set_bo_control_selection(conn,document, reportname, control=controls$name[[1]],all=TRUE)
  close_bo_document(conn, document, save= TRUE)
  df1 <- get_bo_report_data(conn, document, Sys.getenv('BO_TEST_REPORT_NAME'))
  set_bo_control_selection(conn,document, reportname,control=controls$name[[1]],selections=c(3,4))
  close_bo_document(conn, document, save= TRUE)
  df2 <- get_bo_report_data(conn, document, Sys.getenv('BO_TEST_REPORT_NAME'))
  expect_true(nrow(df2) > 0)
  if (!in_httptest2()) {
    expect_gt(nrow(df1), nrow(df2))
  }
})

test_that(paste('Data source specification is retrieved'), {
  skip_if_httptest2()
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  document <- get_bo_item(conn, parent_folder = get_test_folder_id(conn), kind = 'Webi') %>%
    dplyr::filter(stringr::str_detect(`SI_NAME`, Sys.getenv('BO_TEST_DOCUMENT_NAME_PATTERN')))

  provider <- get_bo_data_provider_details(conn, document)
  provider %<>% dplyr::filter(dataSourceType=='unx')
  spc <- get_bo_data_provider_specification(conn, document, provider$id)
  xml <- xml2::read_xml(spc)
  expect_equal(ignore_attr = TRUE, length(xml),2)
})

test_that(paste('Data source specification can be set'), {
  skip_if_httptest2()
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  document <- get_bo_item(conn, parent_folder = get_test_folder_id(conn), kind = 'Webi') %>%
    dplyr::filter(stringr::str_detect(`SI_NAME`, Sys.getenv('BO_TEST_DOCUMENT_NAME_PATTERN')))

  provider <- get_bo_data_provider_details(conn, document)
  provider %<>% dplyr::filter(dataSourceType=='unx')
  spc <- get_bo_data_provider_specification(conn, document, provider$id)
  xml <- xml2::read_xml(spc)
  expect_equal(ignore_attr = TRUE, length(xml),2)
  result <- set_bo_data_provider_specification(conn, document, provider$id, spc)
  expect_no_message('Error')
})

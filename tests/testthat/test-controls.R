test_that(paste("Getting document controls works"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  filename <- Sys.getenv('BO_TEST_EXCEL_FILE_NAME')
  folder_id = Sys.getenv('BO_TEST_FOLDER_ID')
  document <- get_bo_item(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'), owner = NULL, kind = "Webi")
  controls <- get_bo_control_details(conn, document)
  expect_gt(nrow(controls), 0)
  expect_equal(controls$id[1], 'D.IF0')
})

test_that(paste("Getting report controls works"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  filename <- Sys.getenv('BO_TEST_EXCEL_FILE_NAME')
  folder_id = Sys.getenv('BO_TEST_FOLDER_ID')
  document <- get_bo_item(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'), owner = NULL, kind = "Webi")
  controls <- get_bo_control_details(conn, document, Sys.getenv('BO_TEST_REPORT_NAME'))
  expect_gt(nrow(controls), 0)
  expect_equal(controls$id[1], 'R1598.IF1')
})

test_that(paste("Getting document controls selection set works"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  filename <- Sys.getenv('BO_TEST_EXCEL_FILE_NAME')
  folder_id = Sys.getenv('BO_TEST_FOLDER_ID')
  document <- get_bo_item(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'), owner = NULL, kind = "Webi")
  controls <- get_bo_control_details(conn, document)
  set <- get_bo_control_selection_set(conn, document, control = controls$name[[1]])
  expect_gt(length(set), 0)
})

test_that(paste("Getting report controls selection set works"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  filename <- Sys.getenv('BO_TEST_EXCEL_FILE_NAME')
  folder_id = Sys.getenv('BO_TEST_FOLDER_ID')
  document <- get_bo_item(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'), owner = NULL, kind = "Webi")
  controls <- get_bo_control_details(conn, document, Sys.getenv('BO_TEST_REPORT_NAME'))
  set <- get_bo_control_selection_set(conn, document, Sys.getenv('BO_TEST_REPORT_NAME'), controls$name[[1]])
  expect_gt(length(set), 0)
})

test_that(paste("Getting document controls selected items"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  filename <- Sys.getenv('BO_TEST_EXCEL_FILE_NAME')
  folder_id = Sys.getenv('BO_TEST_FOLDER_ID')
  document <- get_bo_item(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'), owner = NULL, kind = "Webi")
  controls <- get_bo_control_details(conn, document)
  selection <- get_bo_control_selection(conn, document, control=controls$name[[1]])
  expect_false(is_empty(selection))
})

test_that(paste("Getting report control selected items"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  filename <- Sys.getenv('BO_TEST_EXCEL_FILE_NAME')
  folder_id = Sys.getenv('BO_TEST_FOLDER_ID')
  reportname <- Sys.getenv('BO_TEST_REPORT_NAME')
  document <- get_bo_item(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'), owner = NULL, kind = "Webi")
  controls <- get_bo_control_details(conn, document, reportname)
  selection <- get_bo_control_selection(conn, document, reportname, controls$name[[1]])
  expect_false(is_empty(selection))
})

test_that(paste("Setting document control selected items"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  filename <- Sys.getenv('BO_TEST_EXCEL_FILE_NAME')
  folder_id = Sys.getenv('BO_TEST_FOLDER_ID')
  document <- get_bo_item(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'), owner = NULL, kind = "Webi")
  controls <- get_bo_control_details(conn, document)
  set_bo_control_selection(conn,document,control=controls$name[[1]],all=TRUE)
  close_bo_document(conn, document, save= TRUE)
  df1 <- get_bo_report_data(conn, document, Sys.getenv('BO_TEST_REPORT_NAME'))
  set_bo_control_selection(conn,document,control=controls$name[[1]],selections=c('6','8'))
  close_bo_document(conn, document, save= TRUE)
  df2 <- get_bo_report_data(conn, document, Sys.getenv('BO_TEST_REPORT_NAME'))
  expect_gt(nrow(df1), nrow(df2))
})

test_that(paste("Setting document control selected items"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  filename <- Sys.getenv('BO_TEST_EXCEL_FILE_NAME')
  folder_id = Sys.getenv('BO_TEST_FOLDER_ID')
  reportname <- Sys.getenv('BO_TEST_REPORT_NAME')
  document <- get_bo_item(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'), owner = NULL, kind = "Webi")
  controls <- get_bo_control_details(conn, document, reportname)
  set_bo_control_selection(conn,document, reportname, control=controls$name[[1]],all=TRUE)
  close_bo_document(conn, document, save= TRUE)
  df1 <- get_bo_report_data(conn, document, Sys.getenv('BO_TEST_REPORT_NAME'))
  set_bo_control_selection(conn,document, reportname,control=controls$name[[1]],selections=c(3,4))
  close_bo_document(conn, document, save= TRUE)
  df2 <- get_bo_report_data(conn, document, Sys.getenv('BO_TEST_REPORT_NAME'))
  expect_gt(nrow(df1), nrow(df2))
})

test_that(paste('Data source specification is retrieved'), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  document <- get_bo_item(conn, parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'), kind = 'Webi') %>%
    dplyr::filter(stringr::str_detect(`SI_NAME`, Sys.getenv('BO_TEST_DOCUMENT_NAME_PATTERN')))

  provider <- get_bo_data_provider_details(conn, document)
  provider %<>% dplyr::filter(dataSourceType=='unx')
  spc <- get_bo_data_provider_specification(conn, document, provider$id)
  xml <- xml2::read_xml(spc)
  expect_equal(length(xml),2)
})

test_that(paste('Data source specification can be set'), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  document <- get_bo_item(conn, parent_folder = Sys.getenv('BO_TEST_FOLDER_ID'), kind = 'Webi') %>%
    dplyr::filter(stringr::str_detect(`SI_NAME`, Sys.getenv('BO_TEST_DOCUMENT_NAME_PATTERN')))

  provider <- get_bo_data_provider_details(conn, document)
  provider %<>% dplyr::filter(dataSourceType=='unx')
  spc <- get_bo_data_provider_specification(conn, document, provider$id)
  xml <- xml2::read_xml(spc)
  expect_equal(length(xml),2)
  result <- set_bo_data_provider_specification(conn, document, provider$id, spc)
  expect_no_message('Error')
})
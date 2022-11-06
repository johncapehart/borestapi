test_that(paste("Document", Sys.getenv('BO_TEST_DOCUMENT_NAME'), "opens"), {
  conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
  document <- get_bo_item_from_name(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), folder_id = Sys.getenv('BO_TEST_FOLDER_ID'), owner = NULL, kind = "Webi")
  expect_equal(document$SI_NAME, Sys.getenv('BO_TEST_DOCUMENT_NAME'))
})

# test_that(paste("Document", Sys.getenv('BO_TEST_DOCUMENT_NAME'), "refreshes"), {
#   conn <- open_bo_connection(server=Sys.getenv('BO_TEST_SERVER'))
#   document <- get_bo_item_from_name(conn, name = Sys.getenv('BO_TEST_DOCUMENT_NAME'), folder_id = Sys.getenv('BO_TEST_FOLDER_ID'), owner = NULL, kind = "Webi")
#   refresh_bo_document(conn,document=document)
# })


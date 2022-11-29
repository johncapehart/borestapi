test_that(paste("connection opens to", Sys.getenv("BO_SERVER")), {
  conn <- open_bo_connection()
  host <- conn$request$headers[['Host']]
  host <- stringr::str_replace_all(host,'\\\\.','.')
  logger::log_info(paste("host is", host))
  expect_equal(ignore_attr = TRUE, host, Sys.getenv("BO_SERVER"))
  expect_true(check_bo_connection_state(conn$request))
})

test_that(paste("connection closes to", Sys.getenv("BO_SERVER")), {
  skip('Closing exhausts named connections')
  skip_if_httptest2()
  conn <- open_bo_connection()
  expect_no_error({close_bo_connection(conn)})
})

test_that(paste("connection opens to", Sys.getenv("BO_SERVER")), {
  conn <- open_bo_connection()
  host <- conn$request$headers[['Host']]
  logger::log_info("host is", host)
  expect_match(host, Sys.getenv("BO_SERVER"))
})

test_that(paste("connection closes to", Sys.getenv("BO_SERVER")), {
  conn <- open_bo_connection()
  expect_no_error({close_bo_connection(conn)})
})

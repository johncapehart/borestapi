test_that(paste("connection opens to", Sys.getenv("BO_SERVER")), {
  conn <- open_bo_connection()
  host <- conn$request$headers[['Host']]
  log_message("host is", host)
  expect_match(host, Sys.getenv("BO_SERVER"))
})

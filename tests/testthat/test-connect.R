test_that(paste("connection opens to", Sys.getenv("BO_SERVER")), {
  conn <- open_bo_connection()
  host <- conn$request$headers[['Host']]
  logger::log_info("host is", host)
  expect_match(host, Sys.getenv("BO_SERVER"))
})

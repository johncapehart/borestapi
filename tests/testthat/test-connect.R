test_that(paste("connection opens to", Sys.getenv("BO_SERVER")), {
  conn <- open_bo_connection()
  host <- conn$request$headers[['Host']]
  mycat("host is", host)
  expect_match(host, Sys.getenv("BO_SERVER"))
})

test_that(paste("password is saved at", Sys.getenv("BO_USER_PASSWORD_KEYRING_ENTRY")), {
  secret <- get_keyring_secret(Sys.getenv("BO_USER_PASSWORD_KEYRING_ENTRY"))
  expect_gt(str_length(secret), 0)
})

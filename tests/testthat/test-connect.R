test_that(paste("password is saved at", 'business objects password'), {
  print(get_keyring_file_password())
  print(Sys.getenv('USER'))
  secret <- get_keyring_secret(Sys.getenv("BO_USERNAME"))
  expect_gt(str_length(secret), 0)
})

test_that(paste("connection opens to", Sys.getenv("BO_SERVER")), {
  conn <- open_bo_connection()
  host <- conn$request$headers[['Host']]
  mycat("host is", host)
  expect_match(host, Sys.getenv("BO_SERVER"))
})

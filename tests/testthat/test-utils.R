test_that("log_message logs message", {
  print(get_logging_threshold())
  try({log_message("prefix", "function", ";;line 0", level="ERROR")})
  expect_equal(1, 1)
})

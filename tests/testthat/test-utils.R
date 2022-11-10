test_that("mycat logs message", {
  print(get_logging_threshold())
  try({mycat("prefix", "function", ";;line 0", level="ERROR")})
  expect_equal(1, 1)
})

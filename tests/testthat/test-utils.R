test_that("logs INFO", {
  init_log()
  logger::log_threshold(logger::INFO)
  try({log_info("prefix", ";d debug message", ";ttest-utils line 3")})
  try({log_debug("prefix", ";d debug message", ";ttest-utils line 3")})
  expect_equal(1, 1)
})

test_that("logs DEBUG", {
  logger::log_threshold(logger::DEBUG)
  try({log_debug("prefix", ";d debug message", ";ttest-utils line 3")})
  expect_equal(1, 1)
})

test_that("logs TRACE", {
  logger::log_threshold(logger::TRACE)
  try({log_trace("prefix", ";d debug message", ";ttest-utils line 3")})
  expect_equal(1, 1)
})

test_that("logs WARN", {
  logger::log_threshold(logger::WARN)
  try({log_warn("prefix", ";d debug message", ";ttest-utils line 3")})
  expect_equal(1, 1)
})

test_that("project environment variables are loaded", {
  expect_equal(Sys.getenv('BO_TEST_ENV_INHERITANCE'), 'project')
})

test_that(paste("Logging threshold restored to", Sys.getenv('BO_TEST_LOGGING_THRESHOLD')), {
  logger::log_threshold(Sys.getenv('BO_TEST_LOGGING_THRESHOLD'))
  expect_equal(Sys.getenv('BO_TEST_LOGGING_THRESHOLD'), attributes(logger::log_threshold())$level)
})

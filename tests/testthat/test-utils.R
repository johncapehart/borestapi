test_that("logs INFO", {
  init_log()
  logger::log_threshold(logger::INFO)
  expect_no_condition({log_info("prefix", ";d debug message", ";ttest-utils line 3")})
})

test_that("logs DEBUG", {
  logger::log_threshold(logger::DEBUG)
  expect_no_condition({log_debug("prefix", ";d debug message", ";ttest-utils line 3")})
})

test_that("logs TRACE", {
  logger::log_threshold(logger::TRACE)
  expect_no_condition({log_trace("prefix", ";d debug message", ";ttest-utils line 3")})
})

test_that("logs WARN", {
  logger::log_threshold(logger::WARN)
  expect_no_condition({log_warn("prefix", ";d debug message", ";ttest-utils line 3")})
})

test_that("project environment variables are loaded", {
  expect_equal(ignore_attr = TRUE, Sys.getenv('BO_TEST_ENV_INHERITANCE'), 'project')
})

test_that(paste("Logging threshold restored to", Sys.getenv('BO_TEST_LOGGING_THRESHOLD')), {
  logger::log_threshold(Sys.getenv('BO_TEST_LOGGING_THRESHOLD'))
  expect_equal(ignore_attr = TRUE, Sys.getenv('BO_TEST_LOGGING_THRESHOLD'), attributes(logger::log_threshold())$level)
})

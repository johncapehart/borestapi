test_that("encryption works", {
  plaintext <- 'The quick brown fox'
  cipher <- encrypt_object(plaintext)
  cleartext <- decrypt_object(cipher)
  expect_equal(ignore_attr = TRUE, plaintext, cleartext)
})

test_that("saving secret works", {
  secret <- 'my secret'
  save_item('username', Sys.getenv('BO_SERVER'), secret, table_name = get_password_table_name())
  result <- get_saved_items('username', Sys.getenv('BO_SERVER'), table_name = get_password_table_name())
  expect_equal(ignore_attr = TRUE, secret, result[['value']])
  remove_item('username', Sys.getenv('BO_SERVER'), table_name = get_password_table_name())
  result <- get_saved_items('username', Sys.getenv('BO_SERVER'), table_name = get_password_table_name())
  expect_equal(ignore_attr = TRUE, nrow(result), 0)
})

test_that("saving password works", {
  password <- 'password'
  set_user_password('username', Sys.getenv('BO_SERVER'), password)
  result <- get_user_password('username', Sys.getenv('BO_SERVER'))
  expect_equal(ignore_attr = TRUE, password, result)
  clear_user_password('username', Sys.getenv('BO_SERVER'))
  result <- get_user_password('username', Sys.getenv('BO_SERVER'))
  expect_equal(ignore_attr = TRUE, is_empty(result), TRUE)
})

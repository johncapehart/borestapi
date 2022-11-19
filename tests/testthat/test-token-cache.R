test_that("encryption works", {
  plaintext <- 'The quick brown fox'
  cipher <- encrypt_object(plaintext)
  cleartext <- decrypt_object(cipher)
  expect_equal(plaintext, cleartext)
})

test_that("saving secret works", {
  secret <- 'my secret'
  save_item('foo', Sys.getenv('BO_SERVER'), secret)
  result <- get_saved_items('foo', Sys.getenv('BO_SERVER'))
  expect_equal(secret, result[['value']])
  remove_item('foo', Sys.getenv('BO_SERVER'))
  result <- get_saved_items('foo', Sys.getenv('BO_SERVER'))
  expect_equal(nrow(result), 0)
})

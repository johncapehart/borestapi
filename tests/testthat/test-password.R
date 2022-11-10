test_that("String can be encrypted", {
  plaintext = 'The quick brown fox'
  ciphertext = encrypt_string(plaintext)
  expect_equal(plaintext, decrypt_string(ciphertext))
})

test_that("Password can be saved to keyring", {
  delete_keyring_secret('foo')
  l0 <- list_keyring()
  set_keyring_secret('foo','bar')
  expect_equal('bar',get_keyring_secret('foo'))
  expect_equal(length(list_keyring()), length(l0) + 1)
  delete_keyring_secret('foo')
  expect_equal(length(list_keyring()), length(l0))
})

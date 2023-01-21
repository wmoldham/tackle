test_that("%nin% works", {
  expect_equal(1 %nin% 1:4, FALSE)
  expect_equal(1 %nin% 2:4, TRUE)
  expect_equal(1:3 %nin% 2:4, c(TRUE, FALSE, FALSE))
  expect_equal(NA %nin% c(NA, 1, 2), FALSE)
  expect_equal(NA %nin% c(1, 2), TRUE)
})

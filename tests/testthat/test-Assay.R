test_that("Assay class constructor works", {
  expect_s4_class(Assay(assay), "Assay")
  x <- subset(assay, select = -conc)
  expect_error(Assay(x), "conc")
  x <- subset(assay, select = -value)
  expect_error(Assay(x), "value")
  expect_error(Assay(assay, \(x) identity(x)), "Model must use")
  expect_s4_class(Assay(assay, \(x) MASS::rlm(value ~ id, data = x)), "Assay")
})

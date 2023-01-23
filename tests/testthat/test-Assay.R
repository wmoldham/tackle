test_that("Assay class constructor works", {
  expect_s4_class(Assay(assay), "Assay")
  x <- subset(assay, select = -conc)
  expect_error(Assay(x), "conc")
  x <- subset(assay, select = -value)
  expect_error(Assay(x), "value")
})

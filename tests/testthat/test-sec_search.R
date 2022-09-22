skip_if(!grepl("R_LIBS", getwd(), fixed = TRUE), "not making requests")

test_that("examples work", {
  results <- sec_search(SICs = 2080, forms = "10-K")
  expect_identical(ncol(results), 18L)
})

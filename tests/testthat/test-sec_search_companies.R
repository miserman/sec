skip_if(!grepl("R_LIBS", getwd(), fixed = TRUE), "not making requests")

test_that("examples work", {
  results <- sec_search_companies(2080)
  expect_identical(ncol(results), 3L)
})

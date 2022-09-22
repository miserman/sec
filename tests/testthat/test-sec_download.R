skip_if(!grepl("R_LIBS", getwd(), fixed = TRUE), "not making requests")

test_that("examples work", {
  results <- sec_search(SICs = 2080, forms = "10-K", limit = 100)
  dir <- tempdir()
  sec_download(results[1:3, ], dir, complete = TRUE)
  expect_true(all(dir.exists(paste0(dir, "/", results$ciks[1:3]))))
})

skip_if(!grepl("R_LIBS", getwd(), fixed = TRUE), "not making requests")

test_that("examples work", {
  submissions <- sec_companies("0000077476")
  expect_identical(names(submissions), "0000077476")

  facts <- sec_companies("0000077476", "facts")
  expect_identical(names(facts), "0000077476")
})

test_that("label_year_range() works", {
  expect_equal(label_year_range(prefix = "FY ", digits = 4L)(2000:2001), c("FY 1999\u20132000", "FY 2000\u20132001"))
  expect_equal(label_year_range()(2001:2002), c("2000\u20131", "2001\u20132"))
})

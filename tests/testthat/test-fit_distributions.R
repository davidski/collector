context("Fit Distributions")
test_that("Lognormal to normal conversion is idempotent", {
  meanlog <- 1.54
  sdlog <- 10.3

  norms <- lognormal_to_normal(meanlog, sdlog)
  lognorms <- normal_to_lognormal(norms$mean, norms$sd)
  expect_equal(meanlog, lognorms$meanlog)
  expect_equal(sdlog, lognorms$sdlog)
})

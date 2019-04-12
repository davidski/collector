context("Fit distributions")

test_that("Lognormal to normal conversion is idempotent", {
  meanlog <- 1.54
  sdlog <- 10.3

  norms <- lognormal_to_normal(meanlog, sdlog)
  lognorms <- normal_to_lognormal(norms$mean, norms$sd)
  expect_equal(meanlog, lognorms$meanlog)
  expect_equal(sdlog, lognorms$sdlog)
})

test_that("combine norm functions", {

  dat <- data.frame(mean = c(10, 20, 30),
                    sd = c(4, 5, 10),
                    weight = c(2, 1, 2))
  results <- combine_norm(dat)
  expect_equivalent(nrow(results), 1)
  expect_equivalent(rowSums(results), 26.6)
})

context("Fit distributions")

test_that("Fit lognorm functions", {
  result <- fit_lognorm(low = .20, high = .50)
  expect_equal(result$sdlog, 0.05, tolerance = 0.01)
})

test_that("Fit poison functions", {
  result <- fit_pois(low = 10, high = 50)
  expect_equal(result$lambda, 0.11, tolerance = 0.01)
})


test_that("Lognormal to normal conversion is idempotent", {
  meanlog <- 1.54
  sdlog <- 10.3

  norms <- lognormal_to_normal(meanlog, sdlog)
  lognorms <- normal_to_lognormal(norms$mean, norms$sd)
  expect_equal(meanlog, lognorms$meanlog)
  expect_equal(sdlog, lognorms$sdlog)
})

test_that("Combine lognorm functions", {
  meanlog <- 1.19
  sdlog <- 1.59
  dat <- data.frame(meanlog = c(1, 1.5),
                    sdlog = c(1, 2),
                    weight = c(2, 1))
  result <- combine_lognorm(dat)
  expect_equal(result$meanlog, meanlog, tolerance = 0.01)
  expect_equal(result$sdlog, sdlog, tolerance = 0.01)
})

test_that("combine norm functions", {

  dat <- data.frame(mean = c(10, 20, 30),
                    sd = c(4, 5, 10),
                    weight = c(2, 1, 2))
  results <- combine_norm(dat)
  expect_equivalent(nrow(results), 1)
  expect_equivalent(rowSums(results), 26.6)
})

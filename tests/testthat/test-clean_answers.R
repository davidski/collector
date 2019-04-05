context("clean_answers")

test_that("clean_answers works", {
  data("mc_capability_answers")
  data("mc_scenario_answers")
  cleaned <- clean_answers(capability_answers = mc_capability_answers,
                           scenario_answers = mc_scenario_answers)
  expect_is(cleaned, "list")
  expect_length(cleaned, 2)
  expect_equivalent(names(cleaned), c("capabilities", "scenarios"))
})

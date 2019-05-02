context("Utilities")

test_that("Read questions", {
  data(mc_domains)
  data("mc_capabilities")
  data("mc_scenarios")
  data("mc_sme_top_domains")
  data("calibration_questions")
  data("mc_threat_communities")

  workdir <- file.path(tempdir(), "collector")
  dir.create(workdir, showWarnings = FALSE)
  readr::write_csv(mc_domains, file.path(workdir, "domains.csv"))
  readr::write_csv(mc_capabilities, file.path(workdir, "capabilities.csv"))
  readr::write_csv(mc_scenarios, file.path(workdir, "scenarios.csv"))
  readr::write_csv(mc_sme_top_domains, file.path(workdir, "sme_top_domains.csv"))
  readr::write_csv(calibration_questions, file.path(workdir, "calibration_questions.csv"))
  readr::write_csv(mc_threat_communities, file.path(workdir, "threat_communities.csv"))

  ques <- read_questions(source_dir = workdir)
  expect_s3_class(ques, "tidyrisk_question_set")
  unlink(workdir, recursive = TRUE)
})

test_that("Read answers", {
  data("mc_capability_answers")
  data("mc_scenario_answers")
  data("mc_calibration_answers")

  workdir <- file.path(tempdir(), "collector")
  dir.create(workdir, showWarnings = FALSE)
  readr::write_csv(mc_capability_answers, file.path(workdir, "capability_answers.csv"))
  readr::write_csv(mc_scenario_answers, file.path(workdir, "scenario_answers.csv"))
  readr::write_csv(mc_calibration_answers, file.path(workdir, "calibration_answers.csv"))

  resp <- read_responses(source_dir = workdir)
  expect_s3_class(resp, "tidyrisk_response_set")
  unlink(workdir, recursive = TRUE)
})

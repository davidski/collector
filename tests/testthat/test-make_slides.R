context("Make Slides")

test_that("make slides", {
  data(calibration_questions)
  data(mc_domains)
  data(mc_scenarios)
  data(mc_capabilities)
  data(mc_sme_top_domains)
  data(mc_threat_communities)

  tmpdir <- tempdir(check = TRUE)

  ques <- tidyrisk_question_set(domains = mc_domains,
                                calibration = calibration_questions,
                                scenarios = mc_scenarios,
                                capabilities = mc_capabilities,
                                expertise =  mc_sme_top_domains,
                                threat_communities = mc_threat_communities)

  make_slides("Natalie Wade", ques, tmpdir)
  file_location <- file.path(tmpdir, "natalie_wade.html")
  expect_true(file.exists(file_location))
  unlink(file_location)
})

context("Make Handouts")

test_that("handouts works", {
  data(calibration_questions)
  data(mc_domains)
  data(mc_scenarios)
  data(mc_capabilities)
  data(mc_sme_top_domains)
  data(mc_threat_communities)

  tmpdir <- tempdir()

  ques <- tidyrisk_question_set(domains = mc_domains,
                                calibration = calibration_questions,
                                scenarios = mc_scenarios,
                                capabilities = mc_capabilities,
                                expertise =  mc_sme_top_domains,
                                threat_communities = mc_threat_communities)

  sme <- "Natalie Wade"
  sme_title <- tolower(gsub(" ", "_", sme))

  make_handouts(sme, ques, tmpdir)

  file_location <- file.path(tmpdir, paste0(sme_title, ".docx"))
  expect_true(file.exists(file_location))

  ans_file_location <- file.path(tmpdir, paste0(sme_title, "_answers", ".docx"))
  expect_true(file.exists(ans_file_location))

  unlink(c(file_location, ans_file_location))
})

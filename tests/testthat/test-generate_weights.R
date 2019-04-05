context("Generate Weights")

test_that("weights are generated", {
  data("calibration_questions")
  data(mc_domains)
  data("mc_scenarios")
  data(mc_capabilities)
  data("mc_sme_top_domains")
  data("mc_threat_communities")
  data("mc_calibration_answers")
  data("mc_scenario_answers")
  data("mc_capability_answers")
  ques <- tidyrisk_question_set(domains = mc_domains,
                                calibration = calibration_questions,
                                scenarios = mc_scenarios,
                                capabilities = mc_capabilities,
                                expertise =  mc_sme_top_domains,
                                threat_communities = mc_threat_communities)
  ans <- tidyrisk_response_set(mc_calibration_answers, mc_scenario_answers, mc_capability_answers)
  weights <- generate_weights(ques, ans)
  expect_s3_class(weights, "tbl")
})

context("Prepare Data")

test_that("Scenario objects are created", {
  data(calibration_questions)
  data(mc_domains)
  data(mc_scenarios)
  data(mc_capabilities)
  data(mc_sme_top_domains)
  data(mc_threat_communities)

  ques <- tidyrisk_question_set(domains = mc_domains,
                                calibration = calibration_questions,
                                scenarios = mc_scenarios,
                                capabilities = mc_capabilities,
                                expertise =  mc_sme_top_domains,
                                threat_communities = mc_threat_communities)

  data(mc_calibration_answers)
  data(mc_scenario_answers)
  data(mc_capability_answers)

  ans <- tidyrisk_response_set(mc_calibration_answers, mc_scenario_answers, mc_capability_answers)

  fitted_scenarios <- fit_scenarios(ans)
  fitted_capabilities <- fit_capabilities(ans)
  fitted_threat_communities <- fit_threat_communities(mc_threat_communities)

  scen_objs <- prepare_data(fitted_scenarios, fitted_capabilities,
                            fitted_threat_communities, ques)
  expect_s3_class(scen_objs[[1]], "tidyrisk_scenario")
  expect_equal(length(scen_objs), 1)
})

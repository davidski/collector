## Regenerate sample data sets
library(evaluator)
library(readr)
library(dplyr)

# read in and save domain mappings
mc_domains <- readr::read_csv(here::here("inst/sample_questions/domains.csv"),
                              col_types = cols(domain = col_character(),
                                               description = col_character(),
                                               active = col_logical(),
                                               domain_id = col_character()))
usethis::use_data(mc_domains, overwrite = TRUE)

# read in capabilities
mc_capabilities <- evaluator::import_capabilities(domains = evaluator::mc_domains)
mc_capabilities <- mc_capabilities[, c("capability_id", "domain_id", "capability")]
usethis::use_data(mc_capabilities, overwrite = TRUE)

# read in capability_answers
mc_capability_answers <- readr::read_csv(here::here("data-raw/capability_answers.csv"),
                                         col_types = readr::cols(sme = readr::col_character(),
                                                          capability_id = readr::col_character(),
                                                          low = readr::col_number(),
                                                          high = readr::col_number(),
                                                          date = readr::col_date()))
usethis::use_data(mc_capability_answers, overwrite = TRUE)

# generate and save threat_communities
mc_threat_communities <- readr::read_csv(here::here("data-raw/threat_communities.csv"))
usethis::use_data(mc_threat_communities, overwrite = TRUE)

# read in and save scenarios
mc_scenarios <- evaluator::import_scenarios(domains = evaluator::mc_domains) %>%
  left_join(mc_threat_communities, by = c("tcomm" = "threat_community")) %>%
  select(scenario_id, scenario, threat_id, domain_id, controls)
usethis::use_data(mc_scenarios, overwrite = TRUE)

# scenario answers
mc_scenario_answers <- readr::read_csv(here::here("data-raw/scenario_answers.csv"))
usethis::use_data(mc_scenario_answers, overwrite = TRUE)

# generate and save calibration_questions
calibration_questions <- readr::read_csv(here::here("data-raw/calibration_questions.csv"))
usethis::use_data(calibration_questions, overwrite = TRUE)

# generate and save calibration_answers
mc_calibration_answers <- readr::read_csv(here::here("data-raw/calibration_answers.csv"))
usethis::use_data(mc_calibration_answers, overwrite = TRUE)

# generate and save sme top domains
mc_sme_top_domains <- readr::read_csv(here::here("data-raw/sme_top_domains.csv"))
usethis::use_data(mc_sme_top_domains, overwrite = TRUE)

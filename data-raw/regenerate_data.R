## Regenerate sample data sets
library(evaluator)

# read in and save domain mappings
domains <- readr::read_csv(here::here("inst/extdata/domains.csv"))
devtools::use_data(domains, overwrite = TRUE)

# read in capabilities
capabilities <- evaluator::import_capabilities(here::here("data-raw/survey.xlsx"),
                                               domains = evaluator::domains)
#devtools::use_data(capabilities, overwrite = TRUE)

# read in capability_answers
#devtools::use_data(capability_answers, overwrite = TRUE)

# read in and save scenarios
scenarios <- evaluator::import_scenarios(here::here("data-raw/survey.xlsx"),
                                         domains = evaluator::domains)
#devtools::use_data(domains, overwrite = TRUE)

# scenario answers
#devtools::use_data(scenario_answers, overwrite = TRUE)

# generate and save threat_communities
devtools::use_data(threat_communities, overwrite = TRUE)

# generate and save calibration_questions
devtools::use_data(calibration_questons, overwrite = TRUE)

# generate and save calibration_answers
devtools::use_data(calibration_answers, overwrite = TRUE)

# generate and save sme top domains
devtools::use_data(sme_top_domains, overwrite = TRUE)


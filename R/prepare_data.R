#' Create one or more quantitative scenarios objects suitable for simulation by `evaluator`
#'
#' Given parameters for the scenarios, threat communities, capabilities, and
#' the question set, generate a list of `tidyrisk_scenario` objects that may be
#' fed into \code{evaluator::run_simulations()} for Monte Carlo simulation.
#'
#' @param scenario_parameters Scenarios with final parameters defined.
#' @param capability_parameters Capabilities with final parameters defined.
#' @param threat_parameters Threat communities with final parameters defined.
#' @param questions A questions object.
#'
#' @importFrom dplyr rename left_join mutate select starts_with pull
#' @importFrom tidyr drop_na
#' @importFrom purrr map pmap
#' @importFrom rlang .data
#' @importFrom evaluator tidyrisk_scenario
#' @return A list of \code{tidyrisk_scenario} objects.
#' @export
#'
#' @examples
#' library(dplyr)
#' data(mc_domains, mc_capabilities, mc_scenarios, mc_sme_top_domains,
#'      calibration_questions, mc_threat_communities)
#' question_set <- tidyrisk_question_set(mc_domains, mc_scenarios, mc_capabilities,
#'                           calibration_questions, mc_sme_top_domains,
#'                           mc_threat_communities)
#' response_set <- tidyrisk_response_set(mc_calibration_answers,
#'                           mc_scenario_answers, mc_capability_answers)
#' sme_weightings <- generate_weights(question_set, response_set)
#' data(mc_scenario_parameters_fitted, mc_capability_parameters_fitted,
#'                           mc_threat_parameters_fitted)
#' scenario_parameters <- left_join(mc_scenario_parameters_fitted, sme_weightings, by = "sme") %>%
#'   combine_scenario_parameters()
#' capability_parameters <- left_join(mc_capability_parameters_fitted, sme_weightings, by = "sme") %>%
#'   combine_capability_parameters()
#' quantitative_scenarios <- prepare_data(scenario_parameters,
#'                                        capability_parameters,
#'                                        mc_threat_parameters_fitted,
#'                                        question_set)
prepare_data <- function(scenario_parameters, capability_parameters,
                         threat_parameters, questions) {

  enforce_tidyrisk_question_set(questions)

  # combine capabilities + scenarios into a single dataframe
  scenario_parameters %>%
    # bring in the scenario descriptions
    dplyr::left_join(questions$scenarios, by = "scenario_id") %>%
    # bring in the domains
    dplyr::left_join(questions$domains, by = "domain_id") %>%
    # add TC info
    dplyr::left_join(threat_parameters, by = "threat_id") %>%
    # massage the dataframe to look like the standard evaluator inputs
    dplyr::select(.data$scenario_id, scenario = .data$scenario,
                  dplyr::starts_with("threat_"), .data$domain_id,
                  controls = .data$controls,
                  # TEF parameters
                  tef_func = .data$frequency_func, tef_meanlog = .data$frequency_meanlog, tef_sdlog = .data$frequency_sdlog,
                  # LM parameters
                  lm_func = .data$impact_func, lm_meanlog = .data$impact_meanlog, lm_sdlog = .data$impact_sdlog, lm_min = .data$impact_min, lm_max = .data$impact_max) %>%
    # the only NAs should be for retired scenarios
    tidyr::drop_na() ->
    scenarios_final

  # actually derive controls
  scenarios_final$diff_params <- purrr::map(
    scenarios_final$controls,
    ~derive_controls(capability_ids = .x,
                     capability_parameters = capability_parameters))

  # create our list columns for tef/tc/lm
  scenarios_final %>%
    dplyr::mutate(
      tef_params = purrr::pmap(with(scenarios_final, list(tef_func, tef_meanlog, tef_sdlog)),
                               ~ list(func = ..1, meanlog = ..2, sdlog = ..3)),
      tc_params = purrr::pmap(with(scenarios_final, list(threat_func, threat_mean, threat_sd, threat_min, threat_max)),
                              ~ list(func = ..1, mean = ..2, sd = ..3, min = ..4, max = ..5)),
      lm_params = purrr::pmap(with(scenarios_final, list(lm_func, lm_meanlog, lm_sdlog, lm_min, lm_max)),
                              ~ list(func = ..1, meanlog = ..2, sdlog = ..3, min = ..4, max = ..5))) %>%
    dplyr::mutate(scenarios = pmap(list(tef_params  = .data$tef_params,
                                        tc_params   = .data$tc_params,
                                        lm_params   = .data$lm_params,
                                        diff_params = .data$diff_params,
                                        model       = "openfair_tef_tc_diff_lm"),
                                   evaluator::tidyrisk_scenario)) %>%
    dplyr::pull(.data$scenarios) -> scenarios_final

  scenarios_final
}

#' Generate the quantified capability parameters for a scenario
#'
#' Based on the \code{\link[evaluator]{derive_controls}} function
#'
#' Creates the difficulty parameters (embedded list) for quantitative
#'   parameters.
#' @param capability_ids Comma-delimited list of capability ids
#' @param capability_parameters Dataframe of fitted and combined capability parameters
#' @seealso \code{\link[evaluator]{derive_controls}}
#' @importFrom stringr str_split_fixed
#' @importFrom dplyr select pull
#' @importFrom purrr pmap
#' @importFrom rlang .data set_names
#'
#' @return A list.
#'
#' @examples
#' NULL
derive_controls <- function(capability_ids, capability_parameters) {
  control_list <- stringr::str_split_fixed(capability_ids, ", ", Inf) %>% unlist()

  capability_parameters[capability_parameters$capability_id %in% control_list, ] %>%
    dplyr::mutate(diff_params = purrr::pmap(
      list(.data$capability_func, .data$capability_mean, .data$capability_sd,
           .data$capability_min, .data$capability_max),
      ~ list(func = ..1, mean = ..2, sd = ..3, min = ..4, max = ..5))) %>%
    dplyr::pull(.data$diff_params) %>%
    rlang::set_names(nm = control_list)
}

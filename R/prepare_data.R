#' Create a one or more quantitative scenarios objects suitable for simulaton by `evaluator`
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
#' @importFrom dplyr rename left_join mutate select starts_with
#' @importFrom tidyr drop_na
#' @importFrom purrr map pmap
#' @importFrom rlang .data
#' @importFrom evaluator tidyrisk_scenario
#' @return A list of \code{tidyrisk_scenario} objects.
#' @export
#'
#' @examples
#' \dontrun{
#' questions <- read_questions()
#' quantitative_scenarios <- prepare_data(scenario_parameters,
#'       capability_parameters, threat_parameters, questions)
#' }
prepare_data <- function(scenario_parameters, capability_parameters,
                         threat_parameters, questions) {

  # combine capabilities + scenarios into a single dataframe
  scenario_parameters %>%
    # bring in the scenario descriptions
    dplyr::left_join(questions$scenarios, by = "scenario_id") %>%
    # bring in the domain IDs
    dplyr::left_join(questions$domains, by = "domain") %>%
    # add TC info
    #dplyr::mutate(tc_func = "stats::rnorm", tc_mean = 50, tc_sd = 3, tcomm = "placeholder") %>%
    dplyr::left_join(threat_parameters, by = "threat_id") %>%
    # massage the dataframe to look like the standard evaluator inputs
    dplyr::select(.data$scenario_id, scenario = .data$Scenario,
                  dplyr::starts_with("tc"), .data$domain_id,
                  controls = .data$capabilities,
                  # TEF parameters
                  tef_func = .data$frequency_func, tef_meanlog = .data$frequency_meanlog, tef_sdlog = .data$frequency_sdlog,
                  # LM parameters
                  lm_func = .data$impact_func, lm_meanlog = .data$impact_meanlog, lm_sdlog = .data$impact_sdlog, lm_min = .data$impact_min, lm_max = .data$impact_max) %>%
    # there should be only one NA, for the retired scenario, drop it
    tidyr::drop_na() ->
    # anywhere we don't have data, put dummy values in for testing
    #replace_na(list(tef_func = "stats::rlnorm", tef_meanlog=3, tef_sdlog=0.541,
    #                lm_func = "stats::rlnorm", lm_meanlog=1, lm_sdlog=10)) ->
    scenarios_final

  # actually derive controls
  scenarios_final$diff_params <- purrr::map(scenarios_final$controls,
                                            ~derive_controls_alt(capability_ids = .x,
                                                                 capabilities = capability_parameters))
  # create our list columns for tef/tc/lm
  scenarios_final %>%
    dplyr::mutate(tef_params = purrr::pmap(with(scenarios_final, list(tef_func, tef_meanlog, tef_sdlog)),
                                    ~ list(func = ..1, meanlog = ..2, sdlog = ..3)),
           tc_params = purrr::pmap(with(scenarios_final, list(tc_func, tc_mean, tc_sd, tc_min, tc_max)),
                                   ~ list(func = ..1, mean = ..2, sd = ..3, min = ..4, max = ..5)),
           lm_params = purrr::pmap(with(scenarios_final, list(lm_func, lm_meanlog, lm_sdlog, lm_min, lm_max)),
                                   ~ list(func = ..1, meanlog = ..2, sdlog = ..3, min = ..4, max = ..5))) %>%
    dplyr::select(-contains("_mean"), -contains("_sd"), -contains("_func")) -> scenarios_final

  scenarios_final
  }

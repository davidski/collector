#' Clean extreme answers
#'
#' You may wish to apply some sanity checking bounds on the responses from
#'   subject matter experts. This function applies a set of predefined
#'   transformations to the scenario and capability responses. Review these
#'   assumptions carefully before using them in your own analysis.
#'
#' Make the following assumptions/modifications
#'
#'   - minimum capacity is 5% (we've thought about it - 90% CI)
#'   - maximum capacity is 95% (we're just about the best - 90% CI)
#'   - minimum loss is 1000 dollars (both low and high)
#'   - scale all impact into thousands of dollars (make normal
#'       decomposition easier, and is in line of the scale of
#'       a strategic analysis)
#'   - set a minimum frequency of once per 10 years (0.1)
#'
#' @param scenario_answers Scenario answers dataframe.
#' @param capability_answers Capability answers dataframe.
#'
#' @return A list of modified scenarios and capabilities.
#' @export
#' @importFrom dplyr mutate if_else
#' @importFrom rlang .data
#'
#' @examples
#' data(mc_capability_answers)
#' data(mc_scenario_answers)
#' clean_answers(mc_scenario_answers, mc_capability_answers)
clean_answers <- function(scenario_answers, capability_answers) {
  cap_ans <- capability_answers %>%
    dplyr::mutate(low = dplyr::if_else(.data$low < .05, .05, .data$low),
           high = dplyr::if_else(.data$high > .95, .95, .data$high),
           high = pmax(.data$high, .data$low))

  sce_ans <- scenario_answers %>%
    # set a floor for minimum impact (both low and high range)
    dplyr::mutate(imp_low = dplyr::if_else(.data$imp_low < 1000,
                                           1000,
                                           .data$imp_low),
           imp_high = dplyr::if_else(.data$imp_high < 1000,
                                     1000,
                                     .data$imp_high)) %>%
    dplyr::mutate(freq_low = dplyr::if_else(.data$freq_low == 0,
                                            0.1,
                                            .data$freq_low),
           freq_high = dplyr::if_else(.data$freq_high == 0,
                                      1,
                                      .data$freq_high))
  list(capabilities = cap_ans,
       scenarios = sce_ans)
}

# Combination functions ---------------------------------------------------

# Weight Assignment -------------------------------------------------------


#' Convert lognormal parameters to normal parameters
#'
#' Given a set of parameters describing a lognormal distribution, return
#'   the parameters of the underlying normal distribution.
#'
#' @param meanlog Mean log.
#' @param sdlog Standard deviation log.
#'
#' @return A list.
#' @export
#' @family distribution fitting functions
#'
#' @examples
#' lognormal_to_normal(meanlog=1, sdlog=3)
lognormal_to_normal <- function(meanlog, sdlog) {
  norm_mean <- exp(meanlog + sdlog^2 / 2)
  norm_sd <- sqrt( (exp(sdlog^2) - 1) * exp(2*meanlog + sdlog^2))
  list(mean = norm_mean, sd = norm_sd)
}

#' Convert normal parameters to lognormal parameters
#'
#' Given parameters that describe a normal distribution, convert them back
#'   to parameters for a lognormal distribution.
#'
#' @param normmean Mean.
#' @param normsd Standard deviation.
#'
#' @return A list.
#' @export
#' @family distribution fitting functions
#'
#' @examples
#' normal_to_lognormal(normmean = 20, normsd = 3)
normal_to_lognormal <- function(normmean, normsd) {
  phi <- sqrt(normsd ^ 2 + normmean ^ 2)
  lognorm_meanlog <- log(normmean ^ 2 / phi)
  lognorm_sdlog <- sqrt(log(phi ^ 2 / normmean ^ 2))
  list(meanlog = lognorm_meanlog, sdlog = lognorm_sdlog)
}

#' Weight a set of lognormal parameters into a single distribution
#'
#' @param dat Dataframe of meanlog, sdlog, min, max, and sdlog.
#'
#' @importFrom dplyr mutate summarize_at mutate
#' @importFrom tidyr nest unnest
#' @importFrom purrr map
#' @importFrom tibble tibble
#' @importFrom rlang .data
#' @return A dataframe.
#' @export
#' @family distribution fitting functions
#'
#' @examples
#' dat <- data.frame(meanlog = c(1, 1.5),
#'                   sdlog = c(1, 2),
#'                   min = 0,
#'                   max = Inf,
#'                   weight = c(2, 1))
#' combine_lognorm_trunc(dat)
combine_lognorm_trunc <- function(dat) {
  stopifnot(is.numeric(dat$weight) & !anyNA(dat$weight))
  dat %>%
    # take our weighted sum
    dplyr::summarize(meanlog = sum(.data$meanlog * .data$weight) / sum(.data$weight),
              sdlog = sum(.data$sdlog * .data$weight) / sum(.data$weight),
              min = sum(.data$min * .data$weight) / sum(.data$weight),
              max = sum(.data$max * .data$weight) / sum(.data$weight))
    #dplyr::summarize(meanlog = log(sum(exp(.data$meanlog) * .data$weight) / sum(.data$weight)),
    #          sdlog = log(sum(exp(.data$sdlog * .data$weight)) / sum(.data$weight)),
    #          min = sum(.data$min * .data$weight) / sum(.data$weight),
    #          max = sum(.data$max * .data$weight) / sum(.data$weight))
}

#' Weight a set of lognormal parameters into a single distribution
#'
#' @param dat A dataframe.
#'
#' @importFrom dplyr mutate summarize_at mutate
#' @importFrom tidyr nest unnest
#' @importFrom purrr map
#' @importFrom tibble tibble
#' @importFrom rlang .data
#' @return A dataframe.
#' @export
#' @family distribution fitting functions
#'
#' @examples
#' dat <- data.frame(meanlog = c(1, 1.5),
#'                   sdlog = c(1, 2),
#'                   weight = c(2, 1))
#' combine_lognorm(dat)
combine_lognorm <- function(dat) {
  stopifnot(is.numeric(dat$weight) & !anyNA(dat$weight))
  dat %>%
    dplyr::summarize(meanlog = log(sum(exp(.data$meanlog) * .data$weight) / sum(.data$weight)),
              sdlog = log(sum(exp(.data$sdlog * .data$weight)) / sum(.data$weight)))
}

#' Weight a set of normal parameters into a single distribution
#'
#' Given a set of arbitrary parameters that includes at least a weight column,
#'   take a weighted average of all the other parameters.
#'
#' @param dat Dataframe of mean, sd and weights.
#'
#' @importFrom dplyr summarize_at
#' @importFrom rlang .data
#' @return A dataframe.
#' @export
#' @family distribution fitting functions
#'
#' @examples
#' dat <- data.frame(mean = c(10, 20, 30),
#'               sd = c(4, 5, 10),
#'               weight = c(2, 1, 2))
#' combine_norm(dat)
combine_norm <- function(dat) {
  dat %>%
    #unnest() %>%
    dplyr::summarize_at(.var = vars(-matches("weight")),
                        .funs = funs(sum(. * .data$weight) / sum(.data$weight)))
    #dplyr::summarize(mean = sum(.data$mean * .data$weight) / sum(.data$weight),
    #          sd = sum(.data$sd * .data$weight) / sum(.data$weight))
}


# Distribution fitting functions ------------------------------------------

#' Generate a sum of squares cost function for optimization
#'
#' This is an internal helper function that generates a sum of squares
#'   cost function for any given `r*` function (e.g. rnorm, rlognorm). The
#'   resulting function is intended to be used by an `optim` call for fitting
#'   quantiles to distribution parameters.
#'
#' @param func A distribution function.
#'
#' @return A function.
#' @export
#' @family distribution fitting functions
#' @importFrom rlang get_expr
#'
#' @examples
#' NULL
generate_cost_function <- function(func) {
  function(x, quant, est, ...) {
    x1 <- x[1]
    x2 <- x[2]
    if (x1 < 0 | x2 < 0) return(NA)
    sum( (rlang::get_expr(func)(quant, x1, x2, ...) - est)^2)
  }
}

#' Find parameters that fit quantile values of an unknown lognormal distribution
#'
#' With a 5th and 95th quantile point estimates, fit a lognormal distribution,
#'   returning the parameters of the distribution.
#'
#' @param low 5th quantile.
#' @param high 95th quantile.
#'
#' @return A dataframe.
#' @export
#' @family distribution fitting functions
#' @importFrom tibble tibble
#' @importFrom stats optim qlnorm
#'
#' @examples
#' NULL
fit_lognorm <- function(low, high) {
  dat <- stats::optim(c(0, 1), generate_cost_function(stats::qlnorm),
               quant = c(.05, .95), est = c(low, high))
  tibble::tibble(func = "stats::rlnorm",
                 meanlog = dat$par[[1]],
                 sdlog = dat$par[[2]])
}

#' Find parameters that fit quantile values of an unknown truncated lognormal distribution
#'
#' With a 5th and 95th quantile point estimates and optional lower and
#'   upper bounds, fit a lognormal distribution, returning the parameters of
#'   the distribution.
#'
#' @param low 5th quantile.
#' @param high 95th quantile.
#' @param min lower bound of support.
#' @param max upper bound of support.
#'
#' @importFrom EnvStats qlnormTrunc
#' @importFrom tibble tibble
#' @importFrom stats optim
#' @return A dataframe.
#' @export
#' @family distribution fitting functions
#'
#' @examples
#' fit_lognorm_trunc(low = 10, high = 50, min = 0, max = 100)
fit_lognorm_trunc <- function(low, high, min = 0, max = Inf) {
  dat <- stats::optim(c(0.01, 1), generate_cost_function(EnvStats::qlnormTrunc),
               quant = c(.05, .95), est = c(low, high), min = min, max = max)
  tibble::tibble(func = "EnvStats::rlnormTrunc",
                 meanlog = dat$par[[1]],
                 sdlog = dat$par[[2]],
                 min = min,
                 max = max)
}

#' Find parameters that fit quantile values of an unknown truncated normal
#'   distribution
#'
#' With a 5th and 95th quantile point estimates and optional lower and
#'   upper bounds, fit a truncated normal distribution, returning the parameters of
#'   the distribution.
#'
#' @param low 5th quantile.
#' @param high 95th quantile.
#' @param min Lower bound of support.
#' @param max Upper bound of support.
#'
#' @importFrom EnvStats qnormTrunc
#' @importFrom tibble tibble
#' @importFrom stats optim
#' @return Dataframe.
#' @export
#' @family distribution fitting functions
#'
#' @examples
#' fit_norm_trunc(low = 10, high = 50, min = 0, max = 100)
fit_norm_trunc <- function(low, high, min = 0, max = Inf) {
  dat <- stats::optim(c(0.01, 1), generate_cost_function(EnvStats::qnormTrunc),
               quant = c(.05, .95), est = c(low, high), min = min, max = max)
  tibble::tibble(func = "EnvStats::rnormTrunc",
                 mean = dat$par[[1]],
                 sd = dat$par[[2]],
                 min = min,
                 max = max)
}

#' Find parameters that fit a poisson distribution.
#'
#' With a 5th and 95th quantile point estimates and optional lower and
#'   upper bounds, fit a poisson distribution, returning the parameters of
#'   the distribution.
#'
#' @param low 5th quantile.
#' @param high 95th quantile.
#'
#' @return A dataframe.
#' @export
#' @family distribution fitting functions
#' @importFrom tibble tibble
#' @importFrom stats optim qpois
#'
#' @examples
#' fit_pois(low = 10, high = 50)
fit_pois <- function(low, high) {
  dat <- stats::optim(c(0.01, 1), generate_cost_function(stats::qpois),
               quant = c(.05, .95), est = c(low, high))
  tibble::tibble(func = "stats::rpois", lambda = dat$par[[1]])
}

#' Fit capability parameters via a geometric mean
#'
#' @param capabilities_answers Answers dataframe.
#'
#' @return A dataframe.
#' @export
#' @family distribution fitting functions
#' @import dplyr
#' @importFrom tidyr nest unnest replace_na
#' @importFrom EnvStats geoMean
#' @importFrom purrr map
#' @importFrom rlang .data
#'
#' @examples
#' NULL
fit_capabilities_geomean <- function(capabilities_answers) {
  capabilities_answers %>% dplyr::group_by(.data$capability_id) %>%
    dplyr::summarise(low = EnvStats::geoMean(.data$low, na.rm = TRUE),
              high = EnvStats::geoMean(.data$high, na.rm = TRUE)) %>%
    tidyr::replace_na(list(low = 1, high = 1)) %>%
    tidyr::nest(.data$low:.data$high) %>%
    dplyr::mutate(capability = purrr::map(.data$data,
                                          ~ fit_norm_trunc(.x$low, .x$high,
                                                           min = 0, max = 100))) %>%
    tidyr::unnest(.data$capability) %>% tidyr::unnest(.data$data)
}

#' Fit scenario parameters by applying a geometric mean
#'
#' @param scenario_answers Scenario answers.
#'
#' @importFrom tidyr nest unnest replace_na
#' @importFrom EnvStats geoMean
#' @import dplyr
#' @importFrom purrr map
#' @importFrom rlang .data
#' @return A dataframe.
#' @export
#' @family distribution fitting functions
#'
#' @examples
#' NULL
fit_scenarios_geomean <- function(scenario_answers) {
  scenario_answers %>% dplyr::group_by(.data$scenario_id) %>%
    dplyr::summarise_at(vars(matches("low|high")), .funs = EnvStats::geoMean, na.rm = TRUE) %>%
    # if we are missing any answers, fill it in with default values
    tidyr::replace_na(list(imp_low = 1, imp_high = 1, freq_low = 1, freq_high = 1)) %>%
    tidyr::nest(.data$imp_low:.data$imp_high) %>%
    dplyr::mutate(impact = purrr::map(.data$data, ~ fit_lognorm(.x$imp_low, .x$imp_high))) %>%
    tidyr::unnest(.data$impact, .sep = "_") %>% unnest(.data$data) %>%
    tidyr::nest(.data$freq_low:.data$freq_high) %>%
    dplyr::mutate(frequency = purrr::map(.data$data, ~ fit_lognorm(.x$freq_low, .x$freq_high))) %>%
    tidyr::unnest(.data$frequency, .sep = "_") %>% tidyr::unnest(.data$data)
}

#' Fit SME scenario estimates to distribution parameters
#'
#' Given a set of subject matter expert estimates for the 5th and 95th
#'   quantiles of impact and frequency of contact for events, calculate the
#'   distribution parameters for TEF and LM. Use a truncated lognormal
#'   distribution for LM (losses cannot be infinite in size) and
#'   for the TEF.
#'
#' @param scenario_answers Scenario answers.
#' @param maximum_impact The absolute maximum potential impact of any
#'   single loss event.
#' @param maximum_impact_factor Maximum impact factor - scaling factor
#'   of a SME's 95 percent maximum loss to limit the impact of any single event.
#' @param maximum_frequency_factor Maximum frequency factor - scaling
#'   factor at which to limit frequency of events.
#'
#' @import dplyr
#' @importFrom tidyr nest unnest
#' @importFrom purrr map
#' @importFrom rlang .data
#' @return A dataframe.
#' @export
#' @family distribution fitting functions
#'
#' @examples
#' NULL
fit_scenarios <- function(scenario_answers, maximum_impact = Inf,
                          maximum_impact_factor = 10,
                          maximum_frequency_factor = 10) {
  scenario_answers %>%
    # first we work on the impact data
    tidyr::nest(.data$imp_low:.data$imp_high) %>%
    dplyr::mutate(impact = purrr::map(.data$data, ~
                                 fit_lognorm_trunc(.x$imp_low, .x$imp_high,
                                                   max = min(.x$imp_high * maximum_impact_factor, maximum_impact)))) %>%
    tidyr::unnest(.data$impact, .sep = "_") %>% tidyr::unnest(.data$data) %>%
    # now process the frequency data
    tidyr::nest(.data$freq_low:.data$freq_high) %>%
    dplyr::mutate(frequency = map(
      .data$data, ~ fit_lognorm_trunc(.x$freq_low, .x$freq_high,
                                      max = .x$freq_high * maximum_frequency_factor))) %>%
    tidyr::unnest(.data$frequency, .sep = "_") %>%
    tidyr::unnest(.data$data) -> sce_ans_fitted
  sce_ans_fitted
}

#' Fit SME capability estimates to distribution parameters
#'
#' @param capability_answers Capability answers.
#'
#' @importFrom tidyr nest unnest
#' @import dplyr
#' @importFrom purrr map
#' @importFrom rlang .data
#' @return A dataframe.
#' @export
#' @family distribution fitting functions
#'
#' @examples
#' NULL
fit_capabilities <- function(capability_answers) {
  capability_answers %>%
    tidyr::nest(.data$low:.data$high) %>%
    dplyr::mutate(capability = purrr::map(.data$data, ~ fit_norm_trunc(.x$low, .x$high,
                                                   min = 0, max = 100))) %>%
    tidyr::unnest(.data$capability, .sep = "_") %>%
    tidyr::unnest(.data$data) -> cap_ans_fitted
  cap_ans_fitted
}

#' Fit each of the threat communities to a distribution
#'
#' @param threat_communities Dataframe of threat communities.
#'
#' @importFrom tidyr nest unnest
#' @import dplyr
#' @importFrom purrr map
#' @importFrom rlang .data
#' @return A dataframe.
#' @export
#' @family distribution fitting functions
#'
#' @examples
#' NULL
fit_threat_communities <- function(threat_communities) {
  threat_communities %>%
    tidyr::nest(.data$low:.data$high) %>%
    dplyr::mutate(threat = purrr::map(.data$data, ~ fit_norm_trunc(.x$low, .x$high,
                                                                   min = 0, max = 100))) %>%
    tidyr::unnest(.data$threat, .sep = "_") %>%
    tidyr::unnest(.data$data) -> threat_fitted
  threat_fitted
}

#' Combine multiple SME distributions into a single unified view
#'
#' Given a dataframe with multiple SME fitted distributions for a single
#'   capability, apply weighting for opinion pooling, and construct a final
#'   combined distribution for each OpenFAIR scenario parameter.
#'
#' @param capability_parameters Fitted individual parameters for capabilities.
#'
#' @importFrom dplyr rename group_by mutate select
#' @importFrom tidyr nest unnest
#' @importFrom purrr map
#' @importFrom rlang .data
#' @return A dataframe.
#' @export
#'
#' @examples
#' NULL
combine_capability_parameters <- function(capability_parameters) {
  capability_parameters %>%
    # put our parameters into the expected format
    dplyr::rename(mean = .data$capability_mean, sd = .data$capability_sd,
                  min = .data$capability_min, max = .data$capability_max) %>%
    # iterate over each id & capability function pair
    dplyr::group_by(.data$capability_id, .data$capability_func) %>%
    # nest up the elements we want to combine
    tidyr::nest(c(.data$mean:.data$sd, .data$weight, .data$min, .data$max)) %>%
    # perform the combination
    dplyr::mutate(capability = purrr::map(.data$data, combine_norm)) %>%
    tidyr::unnest(.data$capability, .sep = "_") %>%
    dplyr::select(-.data$data)
}

#' Combine multiple SME distributions into a single unified view
#'
#' Given a dataframe with multiple SME fitted distributions for a single
#'   scenario, decompose the lognormal distribution into normal parameters,
#'   apply weighting for opinion pooling, and construct a final combined
#'   distribution for each OpenFAIR scenario factor.
#'
#' @param scenario_parameters Fitted scenario factors for individual SMEs.
#'
#' @importFrom dplyr rename group_by mutate select
#' @importFrom tidyr nest unnest
#' @importFrom purrr map
#' @importFrom rlang .data
#' @return A dataframe.
#' @export
#'
#' @examples
#' NULL
combine_scenario_parameters <- function(scenario_parameters) {
  scenario_parameters %>%
    dplyr::rename(meanlog = .data$impact_meanlog, sdlog = .data$impact_sdlog,
           weight = .data$imp_weight, min = .data$impact_min,
           max = .data$impact_max) %>%
    dplyr::group_by(.data$scenario_id, .data$impact_func) %>%
    tidyr::nest(c(.data$meanlog:.data$sdlog, .data$weight, .data$min, .data$max), .key = "data") %>%
    dplyr::mutate(impact = purrr::map(.data$data, combine_lognorm_trunc)) %>%
    tidyr::unnest(.data$impact, .sep = "_", .drop = TRUE) -> combined_sce_impact

  scenario_parameters %>%
    dplyr::rename(meanlog = .data$frequency_meanlog,
                  sdlog = .data$frequency_sdlog,
                  weight = .data$freq_weight,
                  min = .data$frequency_min,
                  max = .data$frequency_max) %>%
    dplyr::group_by(.data$scenario_id, .data$frequency_func) %>%
    tidyr::nest(c(.data$meanlog:.data$sdlog, .data$weight, .data$min, .data$max), .key = "data") %>%
    dplyr::mutate(frequency = purrr::map(.data$data, combine_lognorm_trunc)) %>%
    tidyr::unnest(.data$frequency, .sep = "_", .drop = TRUE) -> combined_sce_frequency

  dplyr::left_join(combined_sce_impact, combined_sce_frequency, by = "scenario_id")
}

#' Read scenario questions
#'
#' Reads in all the questions for which subject matter expert input is
#'   needed. Includes the domains, capabilities, scenarios, calibration
#'   questions, and threat communities.
#'
#' Expects the following files to be present:
#'
#'  * `domains.csv` - Domains
#'    - domain_id, domain
#'  * `capabilities.csv` - Capabilities
#'    - domain_id, capability_id, capability
#'  * `scenarios.csv` - Scenarios
#'    - scenario_id, scenario, threat_id, domain_id, controls
#'  * `sme_top_domains.csv` - SME expertise
#'    - sme, domain1, domain2, domain3, domain4, domain5, domain6, domain7
#'  * `calibration_questions.csv` - Calibration questions
#'  * `threat_communities.csv` - Threat communities
#'    - threat_community, threat_id, definition, low, high
#'
#' @export
#' @param source_dir Directory location to find input files.
#' @param active_only Read in only the active elements, defaults to TRUE.
#' @importFrom readr read_csv col_character col_logical cols col_number
#' @importFrom tidyr gather drop_na
#' @importFrom dplyr filter arrange
#' @importFrom rlang .data
#' @return A \code{\link{tidyrisk_question_set}} object
#'
#' @examples
#' \dontrun{
#' read_questions()
#' }
read_questions <- function(source_dir, active_only = TRUE) {

  # domains
  domains <- {
    dat <- readr::read_csv(file.path(source_dir, "domains.csv"),
                           col_types = readr::cols(domain = readr::col_character(),
                                                   domain_id = readr::col_character())) %>%
      dplyr::arrange(.data$domain)
    if (active_only && "active" %in% names(dat)) {
      dplyr::filter(dat, .data$active != FALSE | is.na(.data$active))
      } else {dat}
  }

  # capabilities
  dat <- readr::read_csv(file.path(source_dir, "capabilities.csv"),
                         col_types = readr::cols(capability = readr::col_character(),
                                                 capability_id = readr::col_character(),
                                                 domain_id = readr::col_character())) %>%
      dplyr::arrange(.data$domain_id, .data$capability_id)
  caps <- if (active_only && "active" %in% names(dat)) {
    dplyr::filter(dat, .data$active != FALSE | is.na(.data$active))
    } else {dat}

  # scenarios
  scenarios <- {
    dat <- readr::read_csv(file.path(source_dir, "scenarios.csv"),
                           col_types = readr::cols(
                             scenario_id =  readr::col_character(),
                             scenario_id =  readr::col_character(),
                             threat_id =  readr::col_character(),
                             domain_id =  readr::col_character(),
                             controls =  readr::col_character())) %>%
      dplyr::arrange(.data$domain_id, .data$scenario_id)
    if (active_only && "active" %in% names(dat)) {
      dplyr::filter(dat, .data$active != FALSE | is.na(.data$active))} else {dat}
  }

  # expertise
  expertise <- readr::read_csv(file.path(source_dir, "sme_top_domains.csv"),
                               col_types = readr::cols(
                                 sme = readr::col_character(),
                                 .default = readr::col_character()),
                               comment = "#") %>%
    tidyr::gather(key = "key", value = "value", -.data$sme) %>%
    tidyr::drop_na()

  # calibration
  calibration <- readr::read_csv(file.path(source_dir, "calibration_questions.csv"),
                                 col_types = readr::cols(.default = readr::col_character()))

  # threat_communities
  threat_communities <- readr::read_csv(file.path(source_dir, "threat_communities.csv"),
                                        col_types = readr::cols(
                                          low = readr::col_number(),
                                          high = readr::col_number(),
                                          .default = readr::col_character()))

  tidyrisk_question_set(domains = domains, capabilities = caps, scenarios = scenarios,
       expertise = expertise, calibration = calibration,
       threat_communities = threat_communities)
}

#' Read all SMEs responses
#'
#' Reads in all the responses recorded to the calibration, scenarios, and
#'   capability questions.
#'
#' Expects the following files to be present:
#'
#'  * `calibration_answers.csv` - Calibration
#'  * `scenario_answers.csv` - Scenarios
#'  * `capability_answers.csv` - Capabilities
#'
#'
#' @param source_dir Directory location where input files are found.
#' @importFrom readr read_csv col_character col_date col_number col_integer cols
#' @importFrom dplyr mutate_at
#' @importFrom tidyr drop_na
#' @importFrom purrr map
#' @importFrom stringr str_extract_all
#' @return A tidyrisk_response_set object
#' @export
#'
#' @examples
#' \dontrun{
#' read_responses()
#' }
read_responses <- function(source_dir = getwd()) {
  cal_ans <- readr::read_csv(file.path(source_dir, "calibration_answers.csv"),
                             col_types = readr::cols(.default = readr::col_character(),
                                                     sme = readr::col_character(),
                                                     calibration_id = readr::col_character(),
                                                     low = readr::col_character(),
                                                     high = readr::col_character(),
                                                     date = readr::col_date())) %>%
    dplyr::mutate_at(c("low", "high"), ~stringr::str_extract_all(., "\\d+") %>%
                                       purrr::map(~ paste(.x, collapse ="")) %>%
                                       as.numeric())

  sce_ans <- readr::read_csv(file.path(source_dir, "scenario_answers.csv"),
                             col_types = readr::cols(
                               sme = readr::col_character(),
                               scenario_id = readr::col_character(),
                               freq_low = readr::col_number(),
                               freq_high = readr::col_number(),
                               imp_low = readr::col_character(),
                               imp_high = readr::col_character(),
                               date = readr::col_date())) %>%
    tidyr::drop_na() %>%
    dplyr::mutate_at(c("imp_low", "imp_high"), ~stringr::str_extract_all(., "\\d+") %>%
                                               purrr::map(~ paste(.x, collapse = "")) %>%
                                               as.numeric())

  cap_ans <- readr::read_csv(file.path(source_dir, "capability_answers.csv"),
                             col_types = readr::cols(
                               sme = readr::col_character(),
                               capability_id = readr::col_character(),
                               low = readr::col_character(),
                               high = readr::col_character(),
                               date = readr::col_date())) %>%
    tidyr::drop_na() %>%
    dplyr::mutate_at(c("low", "high"), ~stringr::str_extract_all(., "[\\d.]+") %>%
                                       purrr::map(~ paste(.x, collapse ="")) %>%
                                       as.numeric())

  tidyrisk_response_set(capability_answers = cap_ans,
            scenario_answers = sce_ans,
            calibration_answers = cal_ans)
}

#' Calculate the prioritized list of domains for a given subject matter expert (SME)
#'
#' Given a \code{\link{tidyrisk_question_set}} object and the name and the
#'   name of a specific SME of interest, create a vector of the domains in
#'   order of priority.
#'
#' @param sme Name of the subject matter expert.
#' @param questions A \code{\link{tidyrisk_question_set}} object.
#'
#' @importFrom dplyr filter arrange distinct pull
#' @importFrom tidyr drop_na
#' @importFrom rlang .data !!
#' @return An ordered vector of the domains for the requested SME.
#' @export
#'
#' @examples
#' \dontrun{
#' questions <- read_questions()
#' get_sme_domains("Sally Expert", questions)
#' }
get_smes_domains <- function(sme, questions) {

  enforce_tidyrisk_question_set(questions)

  doms <- dplyr::filter(questions$expertise, sme == !!sme) %>%
    tidyr::drop_na() %>%
    dplyr::arrange(.data$key) %>%
    dplyr::distinct(.data$value) %>%
    dplyr::pull()

  c(doms, questions$domains[!questions$domains$domain %in% doms,] %>%
      dplyr::pull(.data$domain))
}

#' Check the readability of scenario text
#'
#' Calculate the Flesch-Kincaid score for each scenario and return that score
#'   along with the scenario ID and domain as a tidy dataframe.
#'
#' @param x A `tidyrisk_question_set` object
#'
#' @importFrom quanteda.textstats textstat_readability
#' @importFrom tibble as_tibble
#' @importFrom dplyr arrange desc select bind_cols
#' @importFrom rlang .data
#' @return A dataframe of the scenario id, domain id, and the Flesch-Kincaid readability score for the scenario text.
#' @export
#'
#' @examples
#' \dontrun{
#' questions <- read_questions()
#' check_readability(questions)
#' }
check_readability <- function(x) {
  enforce_tidyrisk_question_set(x)
  x <- x$scenarios
  dplyr::bind_cols(x, quanteda.textstats::textstat_readability(x$scenario, "Flesch.Kincaid")) %>%
    dplyr::arrange(dplyr::desc(.data$Flesch.Kincaid)) %>%
    dplyr::select(.data$scenario_id, .data$domain_id, .data$Flesch.Kincaid)
}

#' Validate that the parameter passed is a \code{\link{tidyrisk_question_set}} object
#'
#' @param x An object
#'
#' @return NULL
#'
#' @examples
#' NULL
enforce_tidyrisk_question_set <- function(x) {
  if (!is_tidyrisk_question_set(x)) {
    stop("Must pass a tidyrisk_question_set object.", call. = FALSE)
  }
  NULL
}

#' Validate that the parameter passed is a \code{\link{tidyrisk_response_set}} object
#'
#' @param x An object
#'
#' @return NULL
#'
#' @examples
#' NULL
enforce_tidyrisk_response_set <- function(x) {
  if (!is_tidyrisk_response_set(x)) {
    stop("Must pass a tidyrisk_response_set object.", call. = FALSE)
  }
  NULL
}

#' Read scenario questions
#'
#' Reads in all the questions for which subject matter expert input is
#'   needed. Includes the domains, capabilities, scenarios, callibration
#'   questions, and threat communities.
#'
#' @export
#' @param source_dir Directory location to find input files.
#' @param active_only Read in only the active elements, defaults to TRUE.
#' @importFrom readr read_csv col_character col_logical cols col_number
#' @importFrom tidyr gather drop_na
#' @import dplyr
#' @importFrom rlang .data
#' @return A questions object
#'
#' @examples
#' \dontrun{
#' read_questions()
#' }
read_questions <- function(source_dir = getwd(), active_only = TRUE) {

  # domains
  domains <- {
    dat <- readr::read_csv(file.path(source_dir, "domains.csv"),
                           col_types = readr::cols(.default = readr::col_character(),
                                                 active = readr::col_logical())) %>%
      dplyr::arrange(.data$domain)
    if (active_only) {dplyr::filter(dat, .data$active != FALSE | is.na(.data$active))} else {dat}
  }

  # capabilities
  dat <- readr::read_csv(file.path(source_dir, "capabilities.csv"),
                         col_types = readr::cols(.default = readr::col_character(),
                                                 active = readr::col_logical())) %>%
      dplyr::arrange(.data$domain, .data$capability_id)
  caps <- if (active_only) { dplyr::filter(dat, .data$active != FALSE |
                                             is.na(.data$active))} else {dat}

  # scenarios
  scenarios <- {
    dat <- readr::read_csv(file.path(source_dir, "scenarios.csv"),
                           col_types = readr::cols(.default = readr::col_character(),
                                                   active = readr::col_logical())) %>%
      dplyr::arrange(.data$domain, .data$scenario_id)
    if (active_only) {
      dplyr::filter(dat, .data$active != FALSE | is.na(.data$active))} else {dat}
  }

  # expertise
  expertise <- readr::read_csv(file.path(source_dir, "sme_top_domains.csv"),
                               col_types = readr::cols(.default = readr::col_character()),
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

  questions(domains = domains, capabilities = caps, scenarios = scenarios,
       expertise = expertise, calibration = calibration,
       threat_communities = threat_communities)
}

#' Read all SMEs answers
#'
#' Reads in all the answers recorded to the calibration, sceanrios, and
#'   capability questions.
#'
#' @param source_dir Directory location where input files are found.
#' @importFrom readr read_csv col_character col_date col_number col_integer cols
#' @import dplyr
#' @importFrom tidyr drop_na
#' @importFrom purrr map
#' @importFrom stringr str_extract_all
#' @return A list.
#' @export
#'
#' @examples
#' \dontrun{
#' read_answers()
#' }
read_answers <- function(source_dir = getwd()) {
  cal_ans <- readr::read_csv(file.path(source_dir, "calibration_answers.csv"),
                             col_types = readr::cols(.default = readr::col_character(),
                                                     sme = readr::col_character(),
                                                     calibration_id = readr::col_character(),
                                                     low = readr::col_character(),
                                                     high = readr::col_character(),
                                                     date = readr::col_date(format = "%m/%d/%y"))) %>%
    dplyr::mutate_at(c("low", "high"), funs(stringr::str_extract_all(., "\\d+") %>%
                                       purrr::map(~ paste(.x, collapse ="")) %>%
                                       as.numeric()))

  sce_ans <- readr::read_csv(file.path(source_dir, "scenario_answers.csv"),
                             col_types = readr::cols(
                               sme = readr::col_character(),
                               scenario_id = readr::col_character(),
                               freq_low = readr::col_number(),
                               freq_high = readr::col_integer(),
                               imp_low = readr::col_character(),
                               imp_high = readr::col_character(),
                               date = readr::col_date(format = "%m/%d/%y"))) %>%
    tidyr::drop_na() %>%
    dplyr::mutate_at(c("imp_low", "imp_high"), funs(stringr::str_extract_all(., "\\d+") %>%
                                               purrr::map(~ paste(.x, collapse ="")) %>%
                                               as.numeric()))

  cap_ans <- readr::read_csv(file.path(source_dir, "capability_answers.csv"),
                             col_types = readr::cols(
                               sme = readr::col_character(),
                               capability_id = readr::col_character(),
                               low = readr::col_character(),
                               high = readr::col_character(),
                               date = readr::col_date(format = "%m/%d/%y"))) %>%
    tidyr::drop_na() %>%
    mutate_at(c("low", "high"), funs(stringr::str_extract_all(., "[\\d.]+") %>%
                                       purrr::map(~ paste(.x, collapse ="")) %>%
                                       as.numeric()))
  list(cap_ans = cap_ans, sce_ans = sce_ans, cal_ans = cal_ans)
}

#' Calculate the prioritized list of domains for a given SME
#'
#' Given a questions object and the name and the name of a specific SME of
#'   interest, create a vector of the domains in order of priority.
#'
#' @param sme Name of SME.
#' @param questions A questions object.
#'
#' @return A vector.
#' @export
#' @import dplyr
#' @importFrom rlang .data
#'
#'
#' @examples
#' \dontrun{
#' questions <- read_questions()
#' get_sme_domains("Sally Expert", questions)
#' }
get_smes_domains <- function(sme, questions) {
  doms <- dplyr::filter(questions$expertise, sme == !!sme) %>%
    dplyr::arrange(.data$key) %>%
    dplyr::distinct(.data$value) %>%
    dplyr::pull()
  c(doms, questions$domains[!questions$domains$domain %in% doms,] %>% dplyr::pull(.data$domain))
}

#' Check the readability of scenario text
#'
#' With a dataframe of scenario text, scenario_id, and domain, generate the Flesch-
#'   Kincaid score and return that score along with the scenario ID and domain
#'   as a tidy dataframe.
#'
#' @param x Scenarios.
#'
#' @return A dataframe.
#' @export
#' @importFrom quanteda textstat_readability
#' @importFrom tibble as_tibble
#' @importFrom dplyr arrange desc select
#' @importFrom rlang .data
#' @return A dataframe of the scenario id, domain, and the Flesch-Kincaid readability score.
#'
#' @examples
#' \dontrun{
#' questions <- read_questions()
#' check_readability(questions)
#' }
check_readability <- function(x) {
  x <- questions$scenarios
  bind_cols(x, quanteda::textstat_readability(x$scenario, "Flesch.Kincaid")) %>%
    dplyr::arrange(dplyr::desc(.data$Flesch.Kincaid)) %>%
    dplyr::select(.data$id, .data$domain, .data$Flesch.Kincaid)
}

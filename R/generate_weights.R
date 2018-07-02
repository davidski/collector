#' Generate a weighting table for SMEs based upon their calibration answers
#'
#' @param calibration Calibration questions with answer key.
#' @param calibration_answers SME responses to calibration questions.
#' @importFrom dplyr mutate_at left_join group_by mutate summarize n case_when arrange
#' @importFrom stringr str_extract_all
#' @importFrom purrr map
#' @return A dataframe.
#' @export
#'
#' @examples
#' NULL
generate_weights <- function(calibration, calibration_answers){
  # convert string formatted calibration answers to numbers
  calibration %>% dplyr::mutate_at("answer", funs(stringr::str_extract_all(., "[\\d.]+") %>%
                                             purrr::map(~ paste(.x, collapse ="")) %>%
                                             as.numeric())) -> dat

  # calculate how many each SME got correct and compare to target
  dplyr::left_join(calibration_answers, dat, by="calibration_id") %>%
    dplyr::mutate(correct = ifelse(.data$low <= .data$answer &
                                     .data$answer <= .data$high, TRUE, FALSE)) %>%
    dplyr::group_by(.data$sme) %>%
    dplyr::summarise(pct_correct = sum(.data$correct)/ n()) %>%
    dplyr::mutate(weight = dplyr::case_when(
                    .data$pct_correct == .9 ~ 4,      # perfectly calibrated, weight 4
                    .data$pct_correct <= .3 ~ 1,      # not well calibrated, weight 1
                    .data$pct_correct <= .6 ~ 2,      # imperfectly calibrated, weight 2
                    TRUE              ~ 3       # partially calibrated, weight 3
                    ),
                  pct_correct = NULL) %>%
    dplyr::arrange(.data$sme) -> weights
  weights
}

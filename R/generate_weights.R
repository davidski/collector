#' Generate a weighting table for SMEs based upon their calibration answers
#'
#' @param questions Questions object.
#' @param responses Responses object
#' @importFrom dplyr mutate_at left_join group_by mutate summarize n case_when arrange
#' @importFrom stringr str_extract_all
#' @importFrom purrr map
#' @return A dataframe.
#' @export
#'
#' @examples
#' NULL
generate_weights <- function(questions, responses){

  enforce_questions(questions)
  enforce_responses(responses)

  # get calibration questions
  calibration <- questions$calibration

  # convert string formatted calibration answers to numbers
  calibration %>% dplyr::mutate_at("answer", funs(stringr::str_extract_all(., "[\\d.]+") %>%
                                             purrr::map(~ paste(.x, collapse ="")) %>%
                                             as.numeric())) -> dat

  # calculate how many each SME got correct and compare to target
  dplyr::left_join(responses$calibration, dat, by = "calibration_id") %>%
    dplyr::mutate(correct = ifelse(.data$low <= .data$answer &
                                     .data$answer <= .data$high, TRUE, FALSE)) %>%
    dplyr::group_by(.data$sme) %>%
    dplyr::summarise(pct_correct = sum(.data$correct) / n()) %>%
    dplyr::mutate(weight = dplyr::case_when(
                    .data$pct_correct >= .9 ~ 4,      # perfectly calibrated, weight 4
                    .data$pct_correct >= .6 ~ 3,      # imperfectly calibrated, weight 3
                    .data$pct_correct >= .3 ~ 2,      # imperfectly calibrated, weight 2
                    TRUE                    ~ 1,      # not well calibrated, weight 1
                    ),
                  pct_correct = NULL) %>%
    dplyr::arrange(.data$sme) -> weights

  weights
}

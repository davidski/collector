#' Generate a weighting table for SMEs based upon their calibration answers
#'
#' @param questions \code{\link{tidyrisk_question_set}} object.
#' @param responses \code{\link{tidyrisk_response_set}} object
#' @importFrom dplyr mutate_at left_join group_by mutate summarize n case_when arrange vars
#' @importFrom stringr str_extract_all
#' @importFrom purrr map
#' @return A dataframe of SMEs and their numerical weighting.
#' @export
#'
#' @examples
#' NULL
generate_weights <- function(questions, responses){

  enforce_tidyrisk_question_set(questions)
  enforce_tidyrisk_response_set(responses)

  # convert string formatted calibration answers to numbers
  responses$calibration %>% dplyr::mutate_at(dplyr::vars(.data$low, .data$high),
                                             ~stringr::str_extract_all(., "[\\d.]+") %>%
                                               purrr::map(~ paste(.x, collapse ="")) %>%
                                               as.numeric()) -> dat

  # calculate how many each SME got correct and compare to target
  dplyr::left_join(dat, questions$calibration, by = "calibration_id") %>%
    dplyr::mutate(correct = ifelse(.data$low <= .data$answer &
                                     .data$answer <= .data$high, TRUE, FALSE)) %>%
    dplyr::group_by(.data$sme) %>%
    dplyr::summarise(pct_correct = sum(.data$correct) / n()) %>%
    dplyr::mutate(weight = dplyr::case_when(
                    .data$pct_correct >= .9 ~ 4L,      # perfectly calibrated, weight 4
                    .data$pct_correct >= .6 ~ 3L,      # imperfectly calibrated, weight 3
                    .data$pct_correct >= .3 ~ 2L,      # imperfectly calibrated, weight 2
                    TRUE                    ~ 1L,      # not well calibrated, weight 1
                    ),
                  pct_correct = NULL) %>%
    dplyr::arrange(.data$sme) -> weights

  weights
}

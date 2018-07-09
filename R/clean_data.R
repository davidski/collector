#' Clean the answers data frames
#'
#' Make the following assumptions/modifications
#'   - minimum capacity is 5% (we've thought about it - 90% CI)
#'   - maximum capacity is 95% (we're just about the best - 90% CI)
#'   - minimum loss is 1000 dollars (both low and high)
#'   - scale all impact into thousands of dollars (make normal
#'       decomposition easier, and is in line of the scale of
#'       a strategic analysis)
#'   - set a minimum frequency of once per 10 years (0.1)
#' @param capability_answers Capability answers.
#' @param scenario_answers Scenario answers.
#'
#' @return A list.
#' @export
#' @import dplyr
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' answers <- read_answers()
#' clean_answers(answers)
#' }
clean_answers <- function(capability_answers, scenario_answers) {
  cap_ans <- capability_answers %>%
    dplyr::mutate(low = if_else(.data$low < 5, 5, .data$low),
           high = if_else(.data$high > 95, 95, .data$high),
           high = pmax(.data$high, .data$low))

  sce_ans <- scenario_answers %>%
    # set a floor for minimum impact (both low and high range)
    mutate(imp_low = if_else(.data$imp_low < 1000, 1000, .data$imp_low),
           imp_high = if_else(.data$imp_high < 1000, 1000, .data$imp_high)) %>%
    #mutate_at(.vars = c("imp_low", "imp_high"), .funs = funs( . / 1000 )) %>%
    mutate(freq_low = if_else(.data$freq_low == 0, 0.1, .data$freq_low),
           freq_high = if_else(.data$freq_high == 0, 1L, .data$freq_high))
  list(capabilities = cap_ans,
       scenarios = sce_ans)
}

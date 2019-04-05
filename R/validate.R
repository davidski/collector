#' Validate a question set
#'
#' Confirms that all scenario data is well formed (internally consistent).
#'   This is a good sanity check after reading in new scenario data to ensure
#'   that no data entry errors occured.
#'
#' @export
#' @param questions a tidyrisk_question_set object
#' @return Invisibly returns TRUE if all checks pass
#'
#' @examples
#' \dontrun{
#' validate(questions)
#' }
validate <- function(questions) {

  enforce_tidyrisk_question_set(questions)

  # check that there is agreement between domains/scenarios/capabilities
  domain_list <- unique(questions$domains$domain_id)
  scenario_list <- unique(questions$scenarios$domain_id)
  capability_list <- unique(questions$capabilities$domain_id)

  if (!setequal(domain_list, scenario_list)) {
    stop("Scenarios and domains disagree.", call. = FALSE)
  }
  if (!setequal(domain_list, capability_list)) {
    stop("capability and domains disagree.", call. = FALSE)
  }


  # look for agreement in threat communities
  threat_list <- unique(questions$threat_communities$threat_id)
  scenario_list <- unique(questions$scenarios$threat_id)
  if (!setequal(threat_list, scenario_list)) {
    stop("threats and scenarios disagree.", call. = FALSE)
  }

  message("All question set validation checks passed!")
  invisible(TRUE)

}

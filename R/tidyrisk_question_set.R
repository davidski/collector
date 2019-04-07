#' @importFrom methods setOldClass
setOldClass("tidyrisk_question_set")

#' Construct a tidyrisk_question_set object
#'
#' \code{new.tidyrisk_question_set} is a low-level constructor that takes a list of dataframes.
#' \code{tidyrisk_question_set} constructs a tidyrisk_question_set object from dataframes.
#' \code{as.tidyrisk_question_set} is a S3 generic that converts existing objects.
#' \code{validate_tidyrisk_question_set} verifies that the data elements are internally consistent.
#'
#' @param ... Individual dataframes
#' @param domains Domains
#' @param calibration Calibration questions
#' @param scenarios Scenario questions
#' @param capabilities Capability questions
#' @param expertise SME expertise
#' @param threat_communities Threat communities
#' @param x object to coerce
#'
#' @examples
#' NULL
tidyrisk_question_set <- function(domains, scenarios, capabilities, calibration,
                      expertise, threat_communities) {
  x <- new_tidyrisk_question_set(list(domains = domains, scenarios = scenarios,
                                      capabilities = capabilities,
                                      calibration = calibration,
                                      expertise = expertise,
                                      threat_communities = threat_communities))
  validate_tidyrisk_question_set(x)
}

#' @export
#' @rdname tidyrisk_question_set
new_tidyrisk_question_set <- function(x) {
  if (!is.list(x)) stop("`x` must be a list", call. = FALSE)
  mandatory_elements <- c("domains", "scenarios", "capabilities", "calibration",
                          "expertise", "threat_communities")
  if (length(setdiff(mandatory_elements, names(x)))) {
    stop(paste0("Missing elements: ",
                paste0(setdiff(mandatory_elements, names(x)),
                       collapse = ", ")), call. = FALSE)
    }
  structure(x, class = "tidyrisk_question_set")
}

#' @export
#' @rdname tidyrisk_question_set
as.tidyrisk_question_set <- function(x, ...) {
  UseMethod("as.tidyrisk_question_set")
}

#' @export
as.data.frame.tidyrisk_question_set <- function(x, ...) {
  x$scenarios
}

#' Test if the object is a tidyrisk_question_set
#'
#' This function returns TRUE for tidyrisk_question_set or sub-classes
#'   thereof, and FALSE for all other objects.
#'
#' @param x An object
#' @export
#' @examples
#' \dontrun{
#' is_tidyrisk_question_set(x)
#' }
is_tidyrisk_question_set <- function(x) {
  inherits(x, "tidyrisk_question_set")
}

#' @export
#' @rdname tidyrisk_question_set
validate_tidyrisk_question_set <- function(x) {

  enforce_tidyrisk_question_set(x)

  # check that there is agreement between domains/scenarios/capabilities
  domain_list <- unique(x$domains$domain_id)
  scenario_list <- unique(x$scenarios$domain_id)
  capability_list <- unique(x$capabilities$domain_id)

  if (!setequal(domain_list, scenario_list)) {
    stop("Scenarios and domains disagree.", call. = FALSE)
  }
  if (!setequal(domain_list, capability_list)) {
    stop("capability and domains disagree.", call. = FALSE)
  }

  # look for agreement in threat communities
  threat_list <- unique(x$threat_communities$threat_id)
  scenario_list <- unique(x$scenarios$threat_id)
  if (!setequal(threat_list, scenario_list)) {
    stop("threats and scenarios disagree.", call. = FALSE)
  }

  x

}


#' @importFrom methods setOldClass
setOldClass("responses")

#' Construct a responses object
#'
#' \code{new.responses} is a low-level constructor that takes a list of dataframes.
#' \code{responses} constructs a responses from dataframes.
#' \code{as.blob} is a S3 generic that converts existing objects.
#'
#' @param ... Individual dataframes
#' @param calibration_answers Calibration responses
#' @param scenario_answers Scenarios responses
#' @param capability_answers Capability responses
#' @param x object to coerce
#'
#' @examples
#' NULL
responses <- function(...) {
  new_responses(list(...))
}

#' @export
#' @rdname responses
new_responses <- function(calibration_answers, scenario_answers, capability_answers) {
  if (!is.data.frame(calibration_answers)) stop("calibration_answers must be a dataframe")
  if (!is.data.frame(scenario_answers)) stop("scenario_answers must be a dataframe")
  if (!is.data.frame(capability_answers)) stop("capability_answers must be a dataframe")
  structure(list(calibration = calibration_answers,
                 scenarios = scenario_answers,
                 capabilities = capability_answers), class = "responses")
}

#' @export
#' @rdname responses
as.responses <- function(x, ...) {
  UseMethod("as.responses")
}

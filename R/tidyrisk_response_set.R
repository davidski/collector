#' Construct a tidyrisk_response_set object
#'
#' \code{new.tidyrisk_response_set} is a low-level constructor that takes a list of dataframes.
#' \code{tidyrisk_response_set} constructs a tidyrisk_response_set from dataframes.
#' \code{as.tidyrisk_response_set} is a S3 generic that converts existing objects.
#'
#' @param ... Individual dataframes
#' @param calibration_answers Calibration tidyrisk_response_set
#' @param scenario_answers Scenarios tidyrisk_response_set
#' @param capability_answers Capability tidyrisk_response_set
#' @param x object to coerce
#'
#' @export
#' @examples
#' NULL
tidyrisk_response_set <- function(calibration_answers, scenario_answers, capability_answers) {
  new_tidyrisk_response_set(calibration_answers, scenario_answers, capability_answers)
}

#' @export
#' @rdname tidyrisk_response_set
new_tidyrisk_response_set <- function(calibration_answers, scenario_answers, capability_answers) {
  if (!is.data.frame(calibration_answers)) stop("calibration_answers must be a dataframe")
  if (!is.data.frame(scenario_answers)) stop("scenario_answers must be a dataframe")
  if (!is.data.frame(capability_answers)) stop("capability_answers must be a dataframe")
  structure(list(calibration = calibration_answers,
                 scenarios = scenario_answers,
                 capabilities = capability_answers), class = "tidyrisk_response_set")
}

#' @export
#' @rdname tidyrisk_response_set
as.tidyrisk_response_set <- function(x, ...) {
  UseMethod("as.tidyrisk_response_set")
}

#' Test if the object is a tidyrisk_response_set
#'
#' This function returns TRUE for tidyrisk_response_set or sub-classes
#'   thereof, and FALSE for all other objects.
#'
#' @param x An object
#' @export
#' @examples
#' \dontrun{
#' is_tidyrisk_response_set(x)
#' }
is_tidyrisk_response_set <- function(x) {
  inherits(x, "tidyrisk_response_set")
}

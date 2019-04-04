#' @importFrom methods setOldClass
setOldClass("questions")

#' Construct a questions object
#'
#' \code{new.questions} is a low-level constructor that takes a list of dataframes.
#' \code{questions} constructs a questions object from dataframes.
#' \code{as.blob} is a S3 generic that converts existing objects.
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
questions <- function(domains, scenarios, capabilities, calibration,
                      expertise, threat_communities) {
  new_questions(list(domains = domains, scenarios = scenarios,
                     capabilities = capabilities, calibration = calibration,
                     expertise = expertise, threat_communities = threat_communities))
}

#' @export
#' @rdname questions
new_questions <- function(x) {
  if (!is.list(x)) stop("`x` must be a list", call. = FALSE)
  mandatory_elements <- c("domains", "scenarios", "capabilities", "calibration",
                          "expertise", "threat_communities")
  if (length(setdiff(mandatory_elements, names(x)))) {
    stop(paste0("Missing elements: ",
                paste0(setdiff(mandatory_elements, names(x)),
                       collapse = ", ")), call. = FALSE)
    }
  structure(x, class = "questions")
}

#' @export
#' @rdname questions
as.questions <- function(x, ...) {
  UseMethod("as.questions")
}

#' @export
as.data.frame.questions <- function(x, ...) {
  x$scenarios
}

#' Test if the object is a questions
#'
#' This function returns TRUE for questions or subclasses therof, and FALSE for all other objects.
#'
#' @param x An object
#' @export
#' @examples
#' \dontrun{
#' is_questions(x)
#' }
is_questions <- function(x) {
  inherits(x, "questions")
}

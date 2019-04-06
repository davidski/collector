#' Calibration questions
#'
#' A dataset of reference trivia questions for calibrating SMEs.
#'
#' @source
#' Common trivia questions drawn from a variety of open source web resources.
#' @format A data frame with 27 rows and 3 variables:
#' \describe{
#'   \item{question}{text of the calibration question}
#'   \item{answer}{answer text to the calibration question}
#'   \item{calibration_id}{unique identifier for the calibration question}
#' }
"calibration_questions"

#' MetroCare Hospital Calibration Answers
#'
#' A dataset of SME answers to calibration questions.
#'
#' @source
#' This is hypothetical information. Any similarity to any other
#' entity is completely coincidental.
#' @format A data frame with 50 rows and 5 variables:
#' \describe{
#'   \item{sme}{name of the subject matter expert}
#'   \item{calibration_id}{unique identifier of the calibration question}
#'   \item{low}{SME's low end estimate}
#'   \item{high}{SME's high end estimate}
#'   \item{date}{date of answer}
#' }
"mc_calibration_answers"

#' MetroCare Hospital Capabilities
#'
#' A dataset of program capabilities.
#'
#' @source
#' This is hypothetical information. Any similarity to any other
#' entity is completely coincidental.
#' @format A data frame with 60 rows and 3 variables:
#' \describe{
#'   \item{capability_id}{unique identifier of the capability}
#'   \item{domain_id}{domain associated with the capability}
#'   \item{capability}{text description of the capability}
#' }
"mc_capabilities"

#' MetroCare Hospital Capability Answers
#'
#' A dataset of SME answers to capabilities.
#'
#' @source
#' This is hypothetical information. Any similarity to any other
#' entity is completely coincidental.
#' @format A data frame with 1 rows and 7 variables:
#' \describe{
#'   \item{sme}{name of the SME}
#'   \item{capability_id}{identifier of the capability}
#'   \item{low}{capability estimate, low}
#'   \item{high}{capability estimate, high}
#'   \item{date}{date of the answer}
#' }
"mc_capability_answers"

#' MetroCare Hospital Scenario Answers
#'
#' A dataset of SME answers to scenarios.
#'
#' @source
#' This is hypothetical information. Any similarity to any other
#' entity is completely coincidental.
#' @format A data frame with 1 rows and 7 variables:
#' \describe{
#'   \item{sme}{name of the SME}
#'   \item{scenario_id}{identifier of the scenario}
#'   \item{freq_low}{frequency estimate, low}
#'   \item{freq_high}{frequency estimate, high}
#'   \item{imp_low}{impact estimate, low}
#'   \item{imp_high}{impact estimate, high}
#'   \item{date}{date of the answer}
#' }
"mc_scenario_answers"

#' MetroCare Hospital Domains
#'
#' A dataset of program domains.
#'
#' @source
#' This is hypothetical information. Any similarity to any other
#' entity is completely coincidental.
#' @format A data frame with 15 rows and 4 variables:
#' \describe{
#'   \item{domain}{domain title}
#'   \item{description}{descriptive text describing the content of the domain}
#'   \item{active}{logical flag indicating whether or not the domain is in use}
#'   \item{domain_id}{unique domain id}
#' }
"mc_domains"

#' MetroCare Hospital SME Top Domains
#'
#' A dataset of focus domains per SME.
#'
#' @source
#' This is hypothetical information. Any similarity to any other
#' entity is completely coincidental.
#' @format A data frame with 5 rows and 7 variables:
#' \describe{
#'   \item{sme}{SME name}
#'   \item{domain1}{SME's first domain}
#'   \item{domain2}{SME's second domain}
#'   \item{domain3}{SME's third domain}
#'   \item{domain4}{SME's fourth domain}
#'   \item{domain5}{SME's fifth domain}
#'   \item{domain6}{SME's sixth domain}
#' }
"mc_sme_top_domains"

#' MetroCare Hospital Threat Communities
#'
#' A dataset of sample threat communities.
#'
#' @source
#' This is hypothetical information. Any similarity to any other
#' entity is completely coincidental.
#' @format A data frame with 6 rows and 7 variables:
#' \describe{
#'   \item{threat_community}{text title of the threat community}
#'   \item{threat_id}{unique identifier}
#'   \item{definition}{text description of the threat community}
#'   \item{low}{threat communities capability, low end}
#'   \item{high}{threat communities capability, high end}
#'   \item{category}{type of the threat community}
#'   \item{action_type}{action type of the threat community}
#' }
"mc_threat_communities"

#' MetroCare Risk Scenarios
#'
#' A dataset of sample risk scenarios.
#'
#' @source
#' This is hypothetical information. Any similarity to any other
#' entity is completely coincidental.
#' @format A data frame with 56 rows and 5 variables:
#' \describe{
#'   \item{scenario_id}{unique identifier}
#'   \item{scenario}{scenario description}
#'   \item{threat_id}{threat community id}
#'   \item{domain_id}{domain id}
#'   \item{controls}{comma separated list of control ids}
#' }
"mc_scenarios"

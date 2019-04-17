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
#' @format A data frame with 35 rows and 3 variables:
#' \describe{
#'   \item{sme}{SME name}
#'   \item{key}{index of domain}
#'   \item{value}{name of domain}
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

#' MetroCare Hospital Scenario Parameters (fitted)
#'
#' A dataset of sample fitted scenario parameters.
#'
#' @source
#' This is hypothetical information. Any similarity to any other
#' entity is completely coincidental.
#' @format A data frame with 280 rows and 17 variables:
#' \describe{
#'   \item{sme}{name of the sme providing the response}
#'   \item{scenario_id}{unique identifier}
#'   \item{date}{date of the response}
#'   \item{impact_func}{function to use for impact sampling}
#'   \item{impact_meanlog}{threat communities capability, high end}
#'   \item{impact_sdlog}{type of the threat community}
#'   \item{impact_min}{action type of the threat community}
#'   \item{impact_max}{action type of the threat community}
#'   \item{imp_low}{action type of the threat community}
#'   \item{imp_high}{action type of the threat community}
#'   \item{frequency_func}{function to use for frequency sampling}
#'   \item{frequency_meanlog}{frequency meanlog}
#'   \item{frequency_sdlog}{frequency standard deviation log}
#'   \item{frequency_min}{frequency minimum}
#'   \item{frequency_max}{frequency maximum}
#'   \item{freq_low}{action type of the threat community}
#'   \item{freq_high}{action type of the threat community}
#' }
"mc_scenario_parameters_fitted"

#' MetroCare Hospital Capability Parameters (fitted)
#'
#' A dataset of sample fitted capability parameters.
#'
#' @source
#' This is hypothetical information. Any similarity to any other
#' entity is completely coincidental.
#' @format A data frame with 300 rows and 10 variables:
#' \describe{
#'   \item{sme}{name of the sme providing the response}
#'   \item{capability_id}{unique identifier}
#'   \item{date}{text description of the threat community}
#'   \item{capability_func}{capability sampling function}
#'   \item{capability_mean}{capability mean}
#'   \item{capability_sd}{capability standard deviation}
#'   \item{capability_min}{capability minimum}
#'   \item{capability_max}{capability maximum}
#'   \item{low}{threat communities capability, high end}
#'   \item{high}{threat communities capability, high end}
#' }
"mc_capability_parameters_fitted"

#' MetroCare Hospital Threat Parameters (fitted)
#'
#' A dataset of sample fitted threat parameters.
#'
#' @source
#' This is hypothetical information. Any similarity to any other
#' entity is completely coincidental.
#' @format A data frame with 8 rows and 12 variables:
#' \describe{
#'   \item{action_type}{action type}
#'   \item{category}{category}
#'   \item{definition}{text description of the threat community}
#'   \item{high}{action type of the threat community}
#'   \item{low}{type of the threat community}
#'   \item{threat_community}{text title of the threat community}
#'   \item{threat_func}{sampling function}
#'   \item{threat_id}{unique identifier}
#'   \item{threat_max}{threat maximum capability}
#'   \item{threat_mean}{threat mean capability}
#'   \item{threat_sd}{threat capability standard deviation}
#'   \item{threat_min}{threat capability minimum}
#' }
"mc_threat_parameters_fitted"

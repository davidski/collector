% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_distributions.R
\name{fit_scenarios}
\alias{fit_scenarios}
\title{Fit SME scenario estimates to distribution parameters}
\usage{
fit_scenarios(scenario_answers, maximum_impact = Inf,
  maximum_impact_factor = 10, maximum_frequency_factor = 10)
}
\arguments{
\item{scenario_answers}{Scenario answers.}

\item{maximum_impact}{The absolute maximum potential impact of any
single loss event.}

\item{maximum_impact_factor}{Maximum impact factor - scaling factor
of a SME's 95 percent maximum loss to limit the impact of any single event.}

\item{maximum_frequency_factor}{Maximum frequency factor - scaling
factor at which to limit frequency of events.}
}
\value{
A dataframe.
}
\description{
Given a set of subject matter expert estimates for the 5th and 95th
  quantiles of impact and frequency of contact for events, calculate the
  distribution parameters for TEF and LM. Use a truncated lognormal
  distribution for LM (losses cannot be infinite in size) and
  for the TEF.
}
\examples{
NULL
}
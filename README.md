
<!-- README.md is generated from README.Rmd. Please edit that file -->

# collector <a href="https://collector.tidyrisk.org"><img alt="collector Logo" title="collector" align="right" src="man/figures/logo.png" height="139"></a>

<!-- badges: start -->

[![Travis Build
Status](https://travis-ci.org/davidski/collector.svg?branch=master)](https://travis-ci.org/davidski/collector)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/davidski/collector?branch=master&svg=true)](https://ci.appveyor.com/project/davidski/collector)
[![Coverage
Status](https://codecov.io/gh/davidski/collector/branch/master/graph/badge.svg)](https://codecov.io/github/davidski/collector?branch=master)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/collector)](https://cran.r-project.org/package=collector)
![downloads](https://cranlogs.r-pkg.org/badges/grand-total/collector)
<!-- badges: end -->

**collector** is an R package for conducting interviews with subject
matter experts (SMEs) on the risk scenarios facing an organization. It
offers functions for the following stages of input collection:

  - generate scenario and capability questions
  - building interview artifacts, including progress card, slide decks,
    and handouts
  - calibration testing, similar to that promoted by Doug Hubbard and
    the FAIR Institute
  - distribution fitting
  - opinion pooling of multiple SMEs into a single representative
    distribution
  - generating quantitative risk scenarios for simulation and reporting
    by [Evaluator](https://evaluator.tidyrisk.org)

## Installation

Collector is now available on CRAN.

``` r
install.packages("collector")
```

If you wish to run the development (and potentially bleeding edge)
version, you can install directly from GitHub via the following
`remotes` command.

``` r
# install.packages("remotes")
remotes::install_github("davidski/collector")
```

## Basic Flow

See the [package website](https://collector.tidyrisk.org) for reference.
The basic flow for preparing for interviews with your SMEs, processing
the results, and generating parameters for simulation via
[evaluator](https://evaluator.tidyrisk.org) is:

1.  Build questions and define SME expertise

2.  Read in the question set. See `read_questions()` for more
    information.
    
    ``` r
    library(collector)
    
    questions <- read_questions()
    ```

3.  Generate materials for interviewing a SME.
    
    ``` r
    output_dir <- tempdir()
    make_handouts("Leader Name", questions, output_dir)
    make_scorecard("Leader Name", questions, output_dir)
    make_slides("Leader Name", questions, output_dir)
    ```

4.  Read in the responses from your SMEs. See `read_responses()`
    documentation for more information.
    
    ``` r
    responses <- read_responses()
    ```

5.  Fit the SME answers to distributions.
    
    ``` r
    scenario_answers_fitted <- fit_scenarios(responses)
    capability_answers_fitted <- fit_capabilities(responses)
    ```

6.  Combine distributions into final parameters, applying weighting
    based on each SMEs level of calibration.
    
    ``` r
    sme_weightings <- generate_weights(questions, responses)
    scenario_parameters <- left_join(scenario_answers_fitted, sme_weightings, by = "sme") %>% 
      combine_scenario_parameters()
    capability_parameters <- left_join(capability_answers_fitted, sme_weightings, by = "sme") %>% 
      combine_capability_parameters()
    ```

7.  Build quantitative scenarios for
    [evaluator](https://evaluator.tidyrisk.org).
    
    ``` r
    scenarios <- prepare_data(scenario_parameters, capability_parameters, 
                              threat_parameters, questions)
    ```

## Contributing

This project is governed by a [Code of
Conduct](https://collector.tidyrisk.org/CODE_OF_CONDUCT.html). By
participating in this project you agree to abide by these terms.

## License

The [MIT License](LICENSE) applies.

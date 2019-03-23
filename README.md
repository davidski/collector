
<!-- README.md is generated from README.Rmd. Please edit that file -->

# collector <img alt="collector Logo" title="collector" align="right" src="man/figures/collector_hex.png" height="139">

[![Travis Build
Status](https://travis-ci.org/davidski/collector.svg?branch=master)](https://travis-ci.org/collector/evaluator)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/davidski/collector?branch=master&svg=true)](https://ci.appveyor.com/project/collector/evaluator)
[![Coverage
Status](https://codecov.io/gh/davidski/collector/branch/master/graph/badge.svg)](https://codecov.io/github/davidski/collector?branch=master)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/collector)](https://cran.r-project.org/package=colector)
![downloads](https://cranlogs.r-pkg.org/badges/grand-total/collector)

**collector** is an R package for conducting interviews with subject
matter experts (SMEs) on the risk scenarios facing an organization. It
offers functions for the following stages of input collection:

  - generate scenario and capability questions
  - building interview artifacts, including progress card, slide decks,
    and handouts
  - calibration testing, similar to that promoted by Doug Hubbard and
    the FAIR Institute
  - opinion pooling
  - distribution fitting
  - generating quantitative data structures for simulation and further
    reporting by [Evaluator](https://evaluator.severski.net)

## Installation

Collector is not currently on CRAN. The following sample code to install
will not work until a release is made to CRAN.

``` r
install.packages("collector")
```

If you wish to run the development (and potentially bleeding edge)
version of Collector, you can install directly from Github via the
following `devtools` command.

``` r
# install.pacakges("devtools")
devtools::install_github("davidski/collector")
```

## Basic Flow

See the [package website](https://collector.severski.net) for reference.
While long form vignettes need to be created, the basic flow for
preparing for inteviews with your SMEs, processing the results, and
generating parameters for simulation via
[evaulator](https://evaluator.severski.net) is:

1.  Build questions and define SME expertise

2.  Read in the questions
    
    ``` r
    library(collector)
    
    questions <- read_questions()
    ```

3.  Generate materials for interviewing a SME
    
    ``` r
    make_handouts("Leader Name", questions, output_dir)
    make_scorecard("Leader Name", questions, output_dir)
    make_slides("Leader Name", questions, output_dir)
    ```

4.  Read in the answers from your SMEs
    
    ``` r
    answers <- read_answers()
    scenario_answers <- answers$sce_ans
    capability_answers <- answers$cap_ans
    ```

5.  Fit the SME answers to distributions.
    
    ``` r
    scenario_answers_fitted <- fit_scenarios(scenario_answers)
    capability_answers_fitted <- fit_capabilities(capability_answers)
    ```

6.  Combine distributions into final parameters, applying weighting
    based on each SMEs level of calibrartion.
    
    ``` r
    scenario_parameters <- combine_scenario_parameters(scenario_answers_fitted)
    capability_parameters <- combine_capability_parameters(capability_answers_fitted)
    ```

7.  Build quantitative parameters for `evaluator`
    
    ``` r
    scenarios <- prepare_data(scenario_parameters, capability_parameters, 
                              threat_parameters, questions)
    ```

## Contributing

This project is governed by a [Code of Conduct](CODE_OF_CONDUCT.md). By
participating in this project you agree to abide by these terms.

## License

The [MIT License](LICENSE) applies.

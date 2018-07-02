#' \code{collector} package
#'
#' Quantified Risk Assessment Data Collection
#'
#' See the README on
#' \href{https://cran.r-project.org/package=collector/README.html}{CRAN}
#' or \href{https://github.com/davidski/collector}{GitHub}
#'
#' @docType package
#' @name collector
#' @importFrom dplyr %>%
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
## technique from Jenny Bryan's googlesheet package
utils::globalVariables(c("."))

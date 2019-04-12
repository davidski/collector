#' \code{collector} package
#'
#' Quantified Information Risk Assessment Data Collection
#'
#' See the online documentation located at
#' \href{https://evaluator.tidyrisk.org/}{https://evaluator.tidyrisk.org/}
#'
#' @docType package
#' @name collector
#' @importFrom dplyr %>%
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
## technique from Jenny Bryan's googlesheet package
utils::globalVariables(c("."))

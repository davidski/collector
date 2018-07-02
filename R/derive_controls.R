#' Generate the quantified capability parameters for a scenario
#'
#' Based on the \code{\link[evaluator]{derive_controls}} function
#'
#' This custom derive_controls function allows for non-numeric capability IDs.
#'
#' Creates the difficulty parameters (embedded dataframe) for quantitative
#' parameters.
#' @param capability_ids A dataframe.
#' @param capabilities A dataframe.
#' @seealso \code{\link[evaluator]{derive_controls}}
#' @importFrom stringi stri_split_fixed
#' @importFrom dplyr select rowwise do pull
#' @importFrom rlang .data
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' NULL
derive_controls_alt <- function(capability_ids, capabilities) {
 control_list <- stringi::stri_split_fixed(capability_ids, ", ") %>% unlist()

 control_list <- capabilities[capabilities$capability_id %in% control_list, ] %>%
   dplyr::select(func = .data$capability_func, mean = .data$capability_mean,
          sd = .data$capability_sd) %>%
   dplyr::rowwise() %>%
   dplyr::do(diff_params = list(func = .$func, mean = .$mean, sd= .$sd)) %>%
   dplyr::pull()

 return(control_list)
}

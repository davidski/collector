#' Create a scorecard for marking progress through domains in an interview
#'
#' Creates a two page PDF with one grid for scenarios and one for capabilities.
#'   Each grid contains a square for each domain. An analyst can mark/stamp
#'   each domain as it is covered in an interview, gamifying progress.
#'
#'   The domains are ordered according to the SME's expertise profile, ensuring
#'   they match the interview order flow.
#'
#' @param sme Name of SME.
#' @param questions \code{\link{tidyrisk_question_set}} object.
#' @param output_dir Directory to place scorecards.
#'
#' @return Invisibly returns the full path to the saved scorecard.
#' @export
#' @importFrom dplyr mutate row_number if_else
#' @importFrom rlang .data
#' @importFrom tibble tibble
#' @importFrom purrr quietly
#' @importFrom stringr str_wrap str_replace_all
#' @import ggplot2
#' @importFrom patchwork wrap_plots
#'
#' @examples
#' \dontrun{
#' questions <- read_questions()
#' make_scorecard("Sally Expert", questions, output_dir = tempdir())
#' }
make_scorecard <- function(sme, questions, output_dir) {

  enforce_tidyrisk_question_set(questions)

  # get ordered scenarios
  values <- get_smes_domains(sme, questions)

  # calculate_dimensions
  n_col <- 4
  rows <- rep(1:(floor(length(values) / n_col)), each = n_col)
  rows <- c(rows, rep(max(rows) + 1, length(values) - length(rows)))

  # make_dataframe
  dat <- tibble::tibble(id = stringr::str_wrap(values, width = 15),
                        row = rows,
                        column = rep_len(1:n_col, length.out = length(values))) %>%
    dplyr::mutate(highlight = dplyr::if_else((row_number() - 1) %% 5 == 0, "Y", "N"))

  # create_plot
  gg <- ggplot(dat, aes_string(x = "column", y = "row", label = "id")) +
    geom_tile(aes(fill = "highlight"), alpha = 0.5, color = "black") +
    scale_fill_manual(values = c("N" = "white", "Y" = "lightslategray"),
                      guide = "none") +
    coord_equal() + geom_text() + scale_y_reverse() +
    theme_void() +
    theme(axis.text = element_blank(), panel.grid = element_blank()) +
    labs(x = NULL, y = NULL, title = "Scenarios",
         subtitle = "Target takt time: 1 minute per response",
         caption = paste0("SME: ", sme))

  # make_capabilities_plot
  gg_cap <- ggplot(dat, aes_string(x = "column", y = "row", label = "id")) +
    geom_tile(aes_string(fill = "highlight"), alpha = 0.5, color = "black") +
    scale_fill_manual(values = c("N" = "white", "Y" = "lightslategray"),
                      guide = "none") +
    coord_equal() + geom_text() + scale_y_reverse() +
    theme_void() +
    theme(axis.text = element_blank(), panel.grid = element_blank()) +
    labs(x = NULL, y = NULL, title = "Capabilities",
         subtitle = "Target takt time: 1 minute per response",
         caption = paste0("SME: ", sme))

  # make_combined_pdf
  combo <- patchwork::wrap_plots(gg, gg_cap, ncol = 1)
  filename <- tolower(sme) %>% stringr::str_replace_all(" ", "_") %>%
    paste0(., "_scorecard.pdf")
  result <- ggplot2::ggsave(combo, filename = file.path(output_dir, filename))
  invisible(result)
}

#' @export
#' @rdname make_scorecard
make_bingo <- function(sme, questions, output_dir = getwd()) {
  .Deprecated("make_scorecard")
  make_scorecard(sme, questions, output_dir = getwd())
}

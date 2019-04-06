#' Create a set of interview handouts for a SME
#'
#' Creates two MS Word documents. One is an `answers` document that contains
#'   the answers to the calibration questions, the other (with the name of the SME)
#'   does not contain answers and is intended to be a visual reference (and possible
#'   take away) for the SME.
#'
#' @param sme Name of the SME.
#' @param questions tidyrisk_question_set object
#' @param output_dir Directory to place output.
#' @param calibration_questions Number of calibration questions to ask.
#'
#' @return NULL
#' @export
#' @importFrom dplyr sample_n select mutate
#' @importFrom tibble tibble add_column
#' @importFrom officer read_docx body_remove body_add_par body_add_toc body_add_break fp_text
#' @importFrom flextable regulartable align autofit width style merge_h add_header body_add_flextable set_header_labels
#' @importFrom purrr walk
#'
#' @examples
#' \dontrun{
#' questions <- read_questions()
#' make_handouts("Sally Expert", questions)
#' }
make_handouts <- function(sme, questions, output_dir = getwd(), calibration_questions = 10) {

  enforce_tidyrisk_question_set(questions)

  # get a sample set of calibrarion questions for this SME
  cal_ques <- questions$calibration %>% dplyr::sample_n(calibration_questions)

  # order_domains
  domain_list <- get_smes_domains(sme, questions)

  # create sme doc
  doc <- officer::read_docx(system.file(package = "collector", "templates",
                                        "template.docx"))
  doc <- officer::body_remove(doc)

  ## Create title page
  doc <- officer::body_add_par(doc, paste0("Risk Assessment - ", sme), style = "Title")
  doc <- officer::body_add_par(x = doc, value = "Table of Contents", style = "heading 1") %>%
    officer::body_add_toc(level = 1)
  doc <- doc %>% officer::body_add_break()

  # create calibration page
  doc <- officer::body_add_par(x = doc, value = "Calibration Questions",
                               style = "heading 1")
  tbl <- cal_ques %>% dplyr::select("Question" = .data$question) %>%
    dplyr::mutate("Low" = NA_character_, "High" = NA_character_) %>%
    flextable::regulartable()
  tbl <- flextable::align(tbl, align = "left", part = "body")
  tbl <- flextable::align(tbl, align = "center", part = "header")
  #tbl <- theme_vanilla(tbl)
  tbl <- flextable::autofit(tbl)
  tbl <- flextable::width(tbl, j = "Question", width = 4)
  tbl <- flextable::width(tbl, j = c("Low", "High"), width = 1)
  tbl <- tbl %>% flextable::style(pr_t = officer::fp_text(font.family = "Calibiri"), part = "all")

  doc <- flextable::body_add_flextable(x = doc, align = "left", tbl)

  # walk the domains
  domain_list %>% purrr::walk(function(d) {
    doc <- doc %>% officer::body_add_break()

    # add the domain heading
    doc <- officer::body_add_par(x = doc,
                                 value = paste("Domain", d, sep =  " - "),
                                 style = "heading 1")
    if (nrow(questions$domains[questions$domains$domain == d, "description"]) > 0) {
      doc <- officer::body_add_par(x = doc, value = questions$domains[
        questions$domains$domain == d, "description"])
    }

    # get the domain id
    dom_id <- questions$domains[questions$domains$domain == d, ]$domain_id

    # add the scenarios
    doc <- officer::body_add_par(x = doc,
                                 value = paste("Scenarios", d, sep =  " - "),
                                 style = "heading 2")
    questions$scenarios[questions$scenarios$domain_id == dom_id, ] %>%
    dplyr::select("ID" = .data$scenario_id, .data$scenario) %>%
      dplyr::mutate("Frequency Low" = NA_character_,
                    "Frequency High" = NA_character_,
                    "Impact Low" = NA_character_,
                    "Impact High" = NA_character_) %>%
      flextable::regulartable() -> tbl
    tbl <- flextable::set_header_labels(tbl, ID = "ID",
                                        scenario = "Scenario",
                                        `Frequency Low` = "Frequency",
                                        `Frequency High` = "Frequency",
                                        `Impact Low` = "Impact",
                                        `Impact High` = "Impact")
    tbl <- flextable::add_header(tbl,
                                 `Frequency Low` = "Events per Year",
                                 `Frequency High` = "Events per Year",
                                 `Impact Low` = "Dollar Cost per Event",
                                 `Impact High` = "Dollar Cost per Event",
                                 top = FALSE)
    tbl <- flextable::add_header(tbl,
                                 `Frequency Low` = "Low",
                                 `Frequency High` = "High",
                                 `Impact Low` = "Low",
                                 `Impact High` = "High",
                                 top = FALSE ) %>%
      flextable::merge_h(part = "header")
    tbl <- flextable::align(tbl, align = "left", part = "body")
    tbl <- flextable::align(tbl, align = "center", part = "header")
    #tbl <- theme_vanilla(tbl)
    tbl <- flextable::autofit(tbl)
    tbl <- flextable::width(tbl, width = 2/3) %>%
      flextable::width(j = "scenario", width = 3) %>%
      flextable::style(pr_t = officer::fp_text(font.family = "Calibiri"), part = "all")
    doc <- flextable::body_add_flextable(x = doc, align = "left", tbl)
    doc <- doc %>% officer::body_add_break()

    # add capabilities
    doc <- officer::body_add_par(x = doc, value = paste("Capabilities", d,
                                                        sep =  " - "),
                                 style = "heading 2")
    questions$capabilities[questions$capabilities$domain_id == dom_id, ] %>%
      dplyr::select("ID" = .data$capability_id, .data$capability) %>%
      tibble::add_column(cap_low = NA_character_, cap_high = NA_character_) %>%
      flextable::regulartable() -> tbl
    tbl <- flextable::set_header_labels(tbl, ID = "ID",
                                        capability = "Capability",
                                        cap_low = "Capability Range",
                                        cap_high = "Capability Range")
    tbl <- flextable::add_header(tbl, `cap_low` = "% Better than World",
                                 `cap_high` = "% Better than World",
                                 top = FALSE)
    tbl <- flextable::add_header(tbl, cap_low = "Low", cap_high = "High", top = FALSE) %>%
      flextable::merge_h(part = "header")
    tbl <- flextable::align(tbl, align = "left", part = "body")
    tbl <- flextable::align(tbl, align = "center", part = "header")
    #tbl <- theme_vanilla(tbl)
    tbl <- flextable::autofit(tbl)
    tbl <- flextable::width(tbl, j = c("cap_low", "cap_high"), width = 2/3) %>%
      flextable::width(j = "capability", width = 3) %>%
      flextable::style(pr_t = officer::fp_text(font.family = "Calibiri"),
                       part = "all")
    doc <- flextable::body_add_flextable(x = doc, align = "left", tbl)
    })

  # save sme document
  filename <- paste0(tolower(sme) %>% stringr::str_replace_all(" ", "_"), ".docx")
  print(doc, target = file.path(output_dir, filename))

  # create answer doc
  doc <- officer::read_docx(system.file(package = "collector", "templates", "template.docx"))
  doc <- officer::body_remove(doc)

  ## Create title page
  doc <- officer::body_add_par(doc, paste0("Risk Assessment - ", sme, " (Answers)"), style = "Title")
  doc <- officer::body_add_par(x = doc, value = "Table of Contents", style = "heading 1") %>%
    officer::body_add_toc(level = 1)
  doc <- doc %>% officer::body_add_break()

  # create calibration page
  doc <- officer::body_add_par(x = doc, value = "Calibration Questions",
                               style = "heading 1")
  tbl <- cal_ques %>%
    dplyr::select("Question" = .data$question, "Answer" = .data$answer) %>%
    flextable::regulartable()
  tbl <- flextable::align(tbl, align = "left", part = "body")
  tbl <- flextable::align(tbl, align = "center", part = "header")
  #tbl <- theme_vanilla(tbl)
  tbl <- flextable::autofit(tbl)
  tbl <- flextable::width(tbl, j = "Question", width = 4)
  tbl <- flextable::width(tbl, j = "Answer", width = 2)
  tbl <- tbl %>% flextable::style(pr_t = officer::fp_text(font.family = "Calibiri"), part = "all")

  doc <- flextable::body_add_flextable(x = doc, align = "left", tbl)

  # walk the domains
  domain_list %>% purrr::walk(function(d) {
    doc <- doc %>% officer::body_add_break()

    # get the domain id
    dom_id <- questions$domains[questions$domains$domain == d, ]$domain_id

    # add the domain heading
    doc <- officer::body_add_par(x = doc,
                                 value = paste("Domain", d, sep =  " - "),
                                 style = "heading 1")
    if (nrow(questions$domains[questions$domains$domain == d, "description"]) > 0) {
      doc <- officer::body_add_par(x = doc, value = questions$domains[questions$domains$domain == d, "description"])
    }

    # add the scenarios
    doc <- officer::body_add_par(x = doc,
                                 value = paste("Scenarios", d, sep =  " - "),
                                 style = "heading 2")
    questions$scenarios[questions$scenarios$domain_id == dom_id, ] %>%
      dplyr::select("ID" = .data$scenario_id, .data$scenario) %>%
      dplyr::mutate("Frequency Low" = NA_character_,
                    "Frequency High" = NA_character_,
                    "Impact Low" = NA_character_,
                    "Impact High" = NA_character_) %>%
      flextable::regulartable() -> tbl
    tbl <- flextable::set_header_labels(tbl, ID = "ID",
                                        scenario = "Scenario",
                                        `Frequency Low` = "Frequency",
                                        `Frequency High` = "Frequency",
                                        `Impact Low` = "Impact",
                                        `Impact High` = "Impact")
    tbl <- flextable::add_header(tbl,
                                 `Frequency Low` = "Events per Year",
                                 `Frequency High` = "Events per Year",
                                 `Impact Low` = "Dollar Cost per Event",
                                 `Impact High` = "Dollar Cost per Event",
                                 top = FALSE)
    tbl <- flextable::add_header(tbl,
                                 `Frequency Low` = "Low",
                                 `Frequency High` = "High",
                                 `Impact Low` = "Low",
                                 `Impact High` = "High",
                                 top = FALSE ) %>%
      flextable::merge_h(part = "header")
    tbl <- flextable::align(tbl, align = "left", part = "body")
    tbl <- flextable::align(tbl, align = "center", part = "header")
    #tbl <- theme_vanilla(tbl)
    tbl <- flextable::autofit(tbl)
    tbl <- flextable::width(tbl, width = 2/3) %>%
      flextable::width(j = "scenario", width = 3) %>%
      flextable::style(pr_t = officer::fp_text(font.family = "Calibiri"), part = "all")
    doc <- flextable::body_add_flextable(x = doc, align = "left", tbl)
    doc <- doc %>% officer::body_add_break()

    # add capabilities
    doc <- officer::body_add_par(x = doc, value = paste("Capabilities", d, sep =  " - "),
                                 style = "heading 2")
    questions$capabilities[questions$capabilities$domain_id == dom_id, ] %>%
      dplyr::select("ID" = .data$capability_id, .data$capability) %>%
      tibble::add_column(cap_low = NA_character_, cap_high = NA_character_) %>%
      flextable::regulartable() -> tbl
    tbl <- flextable::set_header_labels(tbl, ID = "ID",
                                        capability = "Capability",
                                        cap_low = "Capability Range",
                                        cap_high = "Capability Range")
    tbl <- flextable::add_header(tbl, `cap_low` = "% Better than World",
                                 `cap_high` = "% Better than World", top = FALSE)
    tbl <- flextable::add_header(tbl, cap_low = "Low", cap_high = "High", top = FALSE) %>%
      flextable::merge_h(part = "header")
    tbl <- flextable::align(tbl, align = "left", part = "body")
    tbl <- flextable::align(tbl, align = "center", part = "header")
    #tbl <- theme_vanilla(tbl)
    tbl <- flextable::autofit(tbl)
    tbl <- flextable::width(tbl, j = c("cap_low", "cap_high"), width = 2/3) %>%
      flextable::width(j = "capability", width = 3) %>%
      flextable::style(pr_t = officer::fp_text(font.family = "Calibiri"), part = "all")
    doc <- flextable::body_add_flextable(x = doc, align = "left", tbl)
  })

  # save answer document
  filename <- paste0(tolower(sme) %>% stringr::str_replace_all(" ", "_"), "_answers", ".docx")
  print(doc, target = file.path(output_dir, filename))
}

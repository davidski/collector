#' Create interview slides
#'
#' Creates an in-browser slideshow as a visual aide when conducting an
#' interview with a subject matter expert (SME). The slideshow is customized
#' for the SME by placing the domains in the order of preference for that
#' SME.
#'
#' @param sme Name of the SME being interviewed.
#' @param questions A \code{\link{tidyrisk_question_set}} object.
#' @param output_dir Directory location for knitted slides.
#' @param assessment_title Title of the assessment being performed.
#'
#' @return Invisibly returns the full path to the slide file.
#' @export
#' @import xaringan
#' @importFrom rmarkdown render
#' @importFrom stringr str_replace_all str_glue
#'
#' @examples
#' \dontrun{
#' make_slides("Sally Expert", questions, output_dir = tempdir())
#' }
make_slides <- function(sme, questions, output_dir,
                        assessment_title = "Strategic Risk Assessment") {

  enforce_tidyrisk_question_set(questions)

  # ensure output directory is available
  if (!dir.exists(output_dir)) dir.create(output_dir)

  # save questions object
  saveRDS(questions, file.path(output_dir, "questions.rds"))

  # prepare output location with libs subdir, css + image files
  if (!dir.exists(file.path(output_dir, "css"))) dir.create(file.path(output_dir, "css"))
  file.copy(system.file(package = "collector", "css", "styles.css"),
            file.path(output_dir, "css", "styles.css"))
  if (!dir.exists(file.path(output_dir, "libs"))) dir.create(file.path(output_dir, "libs"))
  if (!dir.exists(file.path(output_dir, "img"))) dir.create(file.path(output_dir, "img"))
  file.copy(system.file(package = "collector", "img"),
            file.path(output_dir),
            recursive = TRUE)

  # copy the RMD to our output directory - Yuck!
  file.copy(system.file(package = "collector", "interview.Rmd"),
            file.path(output_dir, "interview.Rmd"), overwrite = TRUE)

  logo_emoji <- "\\U0002696"  # scales emoji

  slides_path <- rmarkdown::render(
    file.path(output_dir, "interview.Rmd"),
    #output_dir = output_dir,
    output_file = paste0(tolower(sme) %>% stringr::str_replace_all(" ", "_"), ".html"),
    knit_root_dir = output_dir,
    quiet = TRUE,
    params = list("sme" = sme,
                  "assessment_title" = stringr::str_glue("{assessment_title}<br>{logo_emoji}"),
                  "domain_list" = get_smes_domains(sme, questions),
                  "questions_file" = file.path(output_dir, "questions.rds")))

  # remove the temporary rds and Rmd files
  file.remove(file.path(output_dir, "questions.rds"))
  file.remove(file.path(output_dir, "interview.Rmd"))

  invisible(slides_path)
}

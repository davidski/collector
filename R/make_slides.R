#' Create interview slides
#'
#' Creates a HTML-format slideshow for conducting and interview.
#'
#' @param sme SME.
#' @param source_dir Directory location for input files.
#' @param output_dir Directory location for knitted slides.
#' @param assessment_title Title of the assessment being performed
#'
#' @return Output of render.
#' @export
#' @importFrom rmarkdown render
#' @importFrom stringr str_replace_all
#' @importFrom glue glue
#'
#' @examples
#' NULL
make_slides <- function(sme, source_dir, output_dir = getwd(), assessment_title = "Strategic Risk Assessment") {

  #read data
  dat <- read_questions(source_dir)

  # ensure output directory is available
  if (!dir.exists(output_dir)) dir.create(output_dir)

  # prepare output location with libs subdir, css + image files
  if (!dir.exists(file.path(output_dir, "css"))) dir.create(file.path(output_dir, "css"))
  file.copy(system.file(package = "collector", "css", "styles.css"),
            file.path(output_dir, "css", "styles.css"))
  if (!dir.exists(file.path(output_dir, "libs"))) dir.create(file.path(output_dir, "libs"))
  if (!dir.exists(file.path(output_dir, "img"))) dir.create(file.path(output_dir, "img"))
  file.copy(system.file(package = "collector", "img"),
            file.path(output_dir),
            recursive = TRUE)

  # copy the RMD to our output direction - Yuck!
  file.copy(system.file(package = "collector", "interview.Rmd"),
            file.path(output_dir, "interview.Rmd"), overwrite = TRUE)

  rmarkdown::render(file.path(output_dir, "interview.Rmd"),
                    #output_dir = output_dir,
                    output_file = paste0(tolower(sme) %>% stringr::str_replace_all(" ", "_"), ".html"),
                    knit_root_dir = output_dir,
                    params=list("sme"= sme,
                                "assessment_title" = glue::glue("{assessment_title}<br>&#2696️"),
                                "domain_list" = get_smes_domains(sme, dat$domains, dat$expertise),
                                "source_dir" = source_dir))
}

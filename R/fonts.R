load_fonts <- function() {
  if (.Platform$OS.type == "windows") {
    windowsFonts <- grDevices::windowsFonts
    extrafont::loadfonts(device = "win", quiet = TRUE)
  }
  pdfFonts <- grDevices::pdfFonts
  extrafont::loadfonts(device = "pdf", quiet = TRUE)

  if (base::requireNamespace("systemfonts", quietly = TRUE)) {
    if ("Raleway" %in% systemfonts::system_fonts()$family) {
      systemfonts::register_variant(
        name = "Raleway KU",
        family = "Raleway",
        features = systemfonts::font_feature(
          ligatures = c("standard", "discretionary"),
          letters = NULL,
          numbers = "lining"
        )
      )
    }
  }
}

#' Choose the first installed out of the list of choices
#'
#' As a side-effect, a warning will be thrown if none of the
#' choices are installed.
#'
#' @param choices character, vector of font family names
#' @param verbose logical, whether to display chosen font
#'
#' @return the first installed font from `choices`, or
#'   failing that, "sans"
#'
#' @examples
#' if (interactive()) {
#'   print(choose_first_installed_font(c("Blah blah", "Arial", "Another made-up font")))
#' }
choose_first_installed_font <- function(choices, verbose = FALSE) {
  # FIXME: Do we want to load them every time or in .onAttach() or both?
  load_fonts()

  choices_installed <- choices[base::which(choices %in% extrafont::fonts())]
  if (base::length(choices_installed) == 0L) {
    base::warning("None of the following fonts are listed in `extrafont::fonts()`: ", format(list(choices)), "\n")
    base::warning("Using font 'sans' instead\n")
    chosen_font <- "sans"
  } else {
    chosen_font <- choices_installed[[1]]
    if (verbose) {
      base::message("Using font '", chosen_font, "'")
    }
  }
  return(chosen_font)
}

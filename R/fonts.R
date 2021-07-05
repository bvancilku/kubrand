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

load_fonts <- function() {
  pdfFonts <- grDevices::pdfFonts
  windowsFonts <- grDevices::windowsFonts
  if (.Platform$OS.type == "windows") {
    extrafont::loadfonts(device = "win", quiet = TRUE)
  }
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

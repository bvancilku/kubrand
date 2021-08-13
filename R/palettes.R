# Menu of palettes
ku_palettes <- base::local({
  # Setup
  set_names <- rlang::set_names
  # Generic color palettes
  cat2_int <- ku_color("KU Blue", "Signature Grey")
  cat2_ext <- ku_color("KU Blue", "Crimson")
  cat3_int <- ku_color("KU Blue", "Signature Grey", "Jayhawk Yellow")
  cat3_ext <- ku_color("KU Blue", "Crimson", "Jayhawk Yellow")
  cat4 <- ku_color("KU Blue", "Crimson", "Jayhawk Yellow", "Signature Grey")
  cat8 <- interleave(cat4, ku_lighten(cat4))
  cat12 <- interleave(ku_darken(cat4), cat4, ku_lighten(cat4))
  cat12a <- ku_color("Night", "KU Blue", "Sky", "Brick", "Crimson", "Fire", "Terra Cotta", "Wheat", "Jayhawk Yellow", "Signature Grey", "Fog", "Steam")
  # Categorical codes
  sex_cat <- c("Female", "Male", "Not Specified")
  career_cat <- c("Undergraduate", "Graduate", "Medical Residents")
  minority_cat <- c("Minority", "Nonminority")
  country_cat <- c("Domestic", "International")
  residency_cat <- c("Resident", "Nonresident")
  course_division_cat <- c("UG Lower Division (<300)", "UG Upper Division (300-699)", "Graduate I (700-899)", "Graduate II (900-999)")
  instruction_mode_cat <- c("In Person", "Distance Learning")
  new_student_type_cat <- c("Others", "First-time Freshmen", "New Transfers")
  # Palettes
  list(
    full = ku_color(),
    # Generic categorical color palettes
    cat2_int = cat2_int,
    cat2_ext = cat2_ext,
    cat3_int = cat3_int,
    cat3_ext = cat3_ext,
    cat4 = cat4,
    cat8 = cat8,
    cat12 = cat12,
    cat12a = cat12a,
    # ku_rainbow = grDevices::colorRampPalette(
    #   ku_lighten(ku_color("Crimson", "Jayhawk Yellow", "KU Blue"), amount = 0.4))(5L),
    red_blue_cat = ku_qualitative(12L, ku_color("Crimson", "KU Blue")),
    light_red_blue_cat = ku_qualitative(12L, ku_lighten(ku_color("Crimson", "KU Blue"), 0.5)),
    # Special purpose categorical color palettes
    sex_int = set_names(cat3_int, sex_cat),
    sex_ext = set_names(cat3_ext, sex_cat),
    career_int = set_names(cat3_int, career_cat),
    career_ext = set_names(cat3_ext, career_cat),
    minority_int = set_names(cat2_int, minority_cat),
    minority_ext = set_names(cat2_ext, minority_cat),
    country_int = set_names(cat2_int, country_cat),
    country_ext = set_names(cat2_ext, country_cat),
    residency_int = set_names(cat2_int, residency_cat),
    residency_ext = set_names(cat2_ext, residency_cat),
    course_division = set_names(cat4, course_division_cat),
    instruction_mode_int = set_names(cat2_int, instruction_mode_cat),
    instruction_mode_ext = set_names(cat2_ext, instruction_mode_cat),
    new_student_type_int = set_names(cat3_int, new_student_type_cat),
    new_student_type_ext = set_names(cat3_ext, new_student_type_cat),
    forecast2 = ku_color("Jayhawk Yellow", "Signature Grey"),
    # Generic sequential color palettes (single hue)
    light_blues = ku_sequential_single_hue(12L, ku_color("KU Blue"), power = 0.75),
    # white_blue = ku_color("White", "KU Blue"),
    sky_night = ku_color("Sky", "Night"),
    limestone_night = ku_color("Limestone", "Night"),
    steam_night = ku_color("Steam", "Night"),
    night_blue_lake_sky = ku_color("Night", "KU Blue", "Lake", "Sky"),
    # Generic sequential color palettes (multiple hue)
    blue_yellow = ku_sequential_multiple_hue(12L, ku_color("KU Blue", "Jayhawk Yellow")),
    fog_fire = ku_sequential_multiple_hue(12L, ku_color("Fire", "Fog"), rev = TRUE),
    # fog_fire = ku_color("Fog", "Fire"),
    # brick_yellow = ku_color("Jayhawk Yellow", "Brick"),
    brick_yellow = ku_sequential_multiple_hue(12L, ku_color("Brick", "Jayhawk Yellow")),
    # night_terracotta = ku_color("Night", "Terra Cotta"),
    night_terracotta = ku_sequential_multiple_hue(12L, ku_color("Night", "Terra Cotta")),
    night_yellow = ku_sequential_multiple_hue(12L, ku_color("Night", "Jayhawk Yellow")),
    # brick_sky = ku_color("Sky", "Brick"),
    brick_sky = ku_sequential_multiple_hue(12L, ku_color("Brick", "Sky")),
    # Generic diverging color palettes
    div_red_blue = ku_color("Brick", "Steam", "KU Blue"),
    div_wheat_sky = ku_color("Wheat", "Steam", "Sky"),
    div_brick_night = ku_color("Brick", "White", "Night")
  )
})
ku_outside_palettes <- list(
  coolwarm = pals::coolwarm,
  warmcool = pals::warmcool,
  ocean.haline = pals::ocean.haline,
  parula = pals::parula,
  brewer.blues = pals::brewer.blues,
  brewer.paired = pals::brewer.paired,
  stepped = pals::stepped
)

#' Access/interpolate from one of the menu of KU palettes
#'
#' @param palette character vector. See `ku_palettes`.
#' @param reverse boolean. Indicates whether to reverse the
#' order.
#' @param ... Additional arguments to pass to `colorRampPalette()`
#'
#' @return a function taking a number of colors and
#' returning a palette
#' @export
#'
#' @examples
#' if (interactive()) {
#'   my_palette <- ku_pal("div_red_blue")
#'   my_palette(8)
#'   show_colors_with_names(my_palette(7), ncol = 1)
#' }
ku_pal <- function(palette = "full", reverse = FALSE, ...) {
  force(palette)
  force(reverse)
  force(list(...))

  if (palette %in% base::names(ku_palettes)) {
    palette <- ku_palettes[[palette]]

    if (reverse) {
      palette <- base::rev(palette)
    }

    grDevices::colorRampPalette(palette, ...)
  } else if (palette %in% base::names(ku_outside_palettes)) {
    palette <- ku_outside_palettes[[palette]]

    if (reverse) {
      function(n) {
        base::rev(palette(n))
      }
    } else {
      palette
    }
  } else {
    valid_palettes <- sort(c(base::names(ku_palettes), base::names(ku_outside_palettes)))
    # TODO: Try misspellings
    base::stop("The palette '", palette, "' is not one of the recognized palettes, which are: ", format(list(valid_palettes), justify = "none"))
  }
}

ku_pals <- function(palettes, ...) {
  palettes %>%
    purrr::map(ku_pal, ...) %>%
    rlang::set_names(palettes)
}

pal_ku <- unikn::newpal(col = ku_palette, names = base::names(ku_palette))
# Try `unikn::seecol(pal_ku)`

# grDevices::windowsFonts(Raleway = grDevices::windowsFont("Raleway"))

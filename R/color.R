
# HCL function factories --------------------------------------------------

hex_to_hcl <- function(hex_code) {
  colorspace::hex2RGB(hex_code) %>%
    methods::as("polarLUV")
}

hex_to_hcl_coords <- function(hex_code) {
  hex_to_hcl(hex_code) %>%
    colorspace::coords()
}

get_hex_to_coord_grabber <- function(coord) {
  force(coord)
  function(hex_code) {
    hex_to_hcl_coords(hex_code)[, coord]
  }
}

hex_to_hue <- get_hex_to_coord_grabber("H")
hex_to_chroma <- get_hex_to_coord_grabber("C")
hex_to_luminance <- get_hex_to_coord_grabber("L")

ku_sequential_single_hue <- function(n, hex1, l2 = 95, rev = TRUE, power = 1.5) {
  colorspace::sequential_hcl(
    n = n,
    h = hex_to_hue(hex1),
    c = hex_to_chroma(hex1),
    l = c(hex_to_luminance(hex1), l2),
    rev = rev,
    power = power
  )
}

ku_sequential_multiple_hue <- function(n, twohex, rev = FALSE) {
  hex1 <- twohex[[1L]]
  hex2 <- twohex[[2L]]
  colorspace::sequential_hcl(
    n = n,
    h1 = hex_to_hue(hex1),
    c1 = hex_to_chroma(hex1),
    l1 = hex_to_luminance(hex1),
    h2 = hex_to_hue(hex2),
    c2 = hex_to_chroma(hex2),
    l2 = hex_to_luminance(hex2),
    rev = rev
  )
}

ku_qualitative <- function(n, two_hex, rev = FALSE) {
  # TODO: I'm using mean and min here. What should I use?
  colorspace::qualitative_hcl(
    n = n,
    h = hex_to_hue(two_hex),
    c = base::mean(hex_to_chroma(two_hex)),
    l = base::min(hex_to_luminance(two_hex)),
    rev = rev
  )
}

# Colors ----------------------------------------------

# Try `scales::show_col(ku_palette)`.
# KU-branded colors
ku_palette <- c(
  # Primary palette
  "KU Blue" = "#0051ba",
  "Crimson" = "#e8000d",
  "Jayhawk Yellow" = "#ffc82d",
  "Signature Grey" = "#85898a",
  # Secondary palette
  "Night" = "#003459",
  "Lake" = "#2767ff",
  "Sky" = "#73cbf2",
  "Brick" = "#971b2f",
  "Fire" = "#ff3042",
  "Wheat" = "#f2a900",
  "Fog" = "#8e9fbc",
  "Steam" = "#dde5ed",
  "Terra Cotta" = "#c66e4e",
  "Limestone" = "#d7d2cb",
  # Implied colors to lighten and darken?
  "White" = "#ffffff",
  "Black" = "#000000"
)

#' Extract colors from the KU palette
#'
#' @param ... character names of colors. See `ku_color()`
#' for available colors.
#'
#' @return named character vector of colors
#' @export
#'
#' @seealso show_colors_with_names can display the palette.
#' @examples
#' if (interactive()) {
#'   print(ku_color())
#'   print(ku_color("KU Blue", "Night"))
#' }
ku_color <- function(...) {
  colors <- c(...)
  if (base::is.null(colors)) {
    return(ku_palette)
  }
  valid_colors <- base::names(ku_palette)
  unrecognized_colors <- base::setdiff(colors, valid_colors)
  if (base::length(unrecognized_colors) > 0) {
    base::warning("Some unrecognized colors were passed to `ku_color`, namely: ", base::paste0('"', unrecognized_colors, '"', collapse = ", "))
    base::message("Valid color names are: ", base::paste0('"', valid_colors, '"', collapse = ", "))
  }

  ku_palette[colors]
}

interleave <- function(...) {
  dots <- list(...)
  interleave_inner <- purrr::lift_dl(
    function(...) c(base::rbind(...))
  )
  rlang::set_names(
    interleave_inner(dots),
    interleave_inner(purrr::map(dots, base::names))
  )
}

ku_mixcolor <- function(color1, color2, interpolation = 0.5) {
  colorspace::hex(colorspace::mixcolor(
    alpha = interpolation,
    color1 = hex_to_hcl(color1),
    color2 = hex_to_hcl(color2),
    where = "polarLUV"
  )) %>%
    rlang::set_names(base::paste0("Mix(", base::names(color1), ", ", base::names(color2), ", interpolation = ", interpolation, ")"))
}

ku_lighten <- function(color1, amount = 0.3) {
  colorspace::lighten(
    col = color1,
    amount = amount,
    method = "relative",
    space = "HCL"
  ) %>%
    rlang::set_names(base::paste0("Lighten(", base::names(color1), ", ", amount, ")"))
}

ku_darken <- function(color1, amount = 0.3) {
  colorspace::darken(
    col = color1,
    amount = amount,
    method = "relative",
    space = "HLS"
  ) %>%
    rlang::set_names(base::paste0("Darken(", base::names(color1), ", ", amount, ")"))
}

#' Show a vector of hexcode colors using names.
#'
#' Modelled off of `scales::show_col` For an alternative to
#' `show_colors_with_names(ku_palette)`, try
#' `colorspace::swatchplot(as.list(ku_palette))`
#'
#' @param colours character, vector of RGB hex codes with names for the colors
#' @param labels logical, whether to display color name labels
#' @param borders color to show for borders
#' @param cex_label size for label
#' @param ncol number of columns (default: `NULL` means to make the output as square as possible.)
#'
#' @return NULL
#' @export
#'
#' @examples
#' if (interactive()) {
#'   show_colors_with_names(ku_palette)
#' }
show_colors_with_names <- function(colours, labels = TRUE, borders = NULL, cex_label = 1, ncol = NULL) {
  n <- length(colours)
  if (base::is.null(ncol)) {
    ncol <- ceiling(sqrt(n))
  }
  nrow <- ceiling(n / ncol)
  colour_labels <- if (base::is.null(base::names(colours))) {
    colours
  } else {
    base::paste0(base::names(colours), "\n", colours)
  }
  colour_labels <- c(colour_labels, rep(NA, nrow * ncol - length(colours)))
  colours <- c(colours, rep(NA, nrow * ncol - length(colours)))
  colours <- matrix(colours, ncol = ncol, byrow = TRUE)
  old <- graphics::par(pty = "s", mar = c(0, 0, 0, 0))
  on.exit(graphics::par(old))
  size <- max(dim(colours))
  base::plot(c(0, dim(colours)[[2]]), c(0, -dim(colours)[[1]]), type = "n", xlab = "", ylab = "", axes = FALSE)
  graphics::rect(
    col(colours) - 1,
    -row(colours) + 1,
    col(colours),
    -row(colours),
    col = colours,
    border = borders
  )
  if (labels) {
    hcl <- farver::decode_colour(colours, "rgb", "hcl")
    label_col <- ifelse(
      hcl[, "l"] > 50,
      "black",
      "white"
    )
    colour_labels <- base::matrix(colour_labels, ncol = ncol, byrow = TRUE)
    graphics::text(
      col(colours) - 0.5,
      -row(colours) + 0.5,
      colour_labels,
      cex = cex_label,
      col = label_col
    )
  }
}

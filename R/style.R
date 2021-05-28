# While we are developing this, be sure to run this:
# source("R/dev/packages.R")

# Useful resources:
# * https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2
# * https://github.com/hneth/unikn


# Utilities -------------------------------------------


hex_to_hcl_coords <- function(hex_code) {
  colorspace::hex2RGB(hex_code) %>%
    methods::as("polarLUV") %>%
    colorspace::coords()
}

get_hex_to_coord_grabber <- function(coord) {
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

ku_mixcolor <- function(color1, color2) {
  colorspace::hex(colorspace::mixcolor(
    alpha = 1.0,
    color1 = colorspace::hex2RGB(color1),
    color2 = colorspace::hex2RGB(color2),
    where = "HCL"
  )) %>%
    rlang::set_names(base::paste0("mixture of ", base::names(color1), " and ", base::names(color2)))
}

ku_lighten <- function(color1, amount = 0.3) {
  colorspace::lighten(
    col = color1,
    amount = amount,
    method = "relative",
    space = "HCL"
  ) %>%
    rlang::set_names(base::paste0("Lightened(", base::round(amount, digits = 2), ") ", base::names(color1)))
}

ku_darken <- function(color1, amount = 0.3) {
  colorspace::darken(
    col = color1,
    amount = amount,
    method = "relative",
    space = "HLS"
  ) %>%
    rlang::set_names(base::paste0("Lightened(", base::round(amount, digits = 2), ") ", base::names(color1)))
}

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
    div_wheat_sky = ku_color("Wheat", "Steam", "Sky")
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
    valid_palettes <- c(base::names(ku_palettes), base::names(ku_outside_palettes))
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

show_colors_with_names <- function(colours, labels = TRUE, borders = NULL, cex_label = 1, ncol = NULL) {
  # Modeled off of scales::show_col

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
  old <- par(pty = "s", mar = c(0, 0, 0, 0))
  on.exit(par(old))
  size <- max(dim(colours))
  plot(c(0, dim(colours)[[2]]), c(0, -dim(colours)[[1]]), type = "n", xlab = "", ylab = "", axes = FALSE)
  rect(
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
    text(
      col(colours) - 0.5,
      -row(colours) + 0.5,
      colour_labels,
      cex = cex_label,
      col = label_col
    )
  }
}


# ggplot2 theme ---------------------------------------


setup_theme_ku <- function() {
  old <- ggplot2:::check_subclass("point", "Geom")$default_aes

  ggplot2::update_geom_defaults(
    "point",
    list(
      colour = NULL # FIXME: Set geom default color
    )
  )

  old
}

#' KU branded {ggplot2} theme
#'
#' TODO:
#' - Think about effect of `ggplot2::coord_cartesian(expand = FALSE)`
#'   - And also maybe add clip = 'off'
#' - Add spacing between title and subtitle.
#' - Consider adding KU logo watermark in corner (with option in function to turn it off).
#' - Lighten the axis tick labels.
#'
#'
#' @param base_size
#' @param base_family
#' @param base_line_size
#' @param base_rect_size
#' @param title_location character, c("plot", "pane")
#'
#' @return
#' @export
#'
#' @examples
theme_ku <- function(base_size = 10,
                     base_family = "sans",
                     base_line_size = base_size / 22,
                     base_rect_size = base_size / 22,
                     title_location = "plot") {
  size_factor <- 1.2
  base_text_color <- ku_color("Night")
  axis_text_color <- ku_color("Signature Grey")
  base_grid_color <- ku_lighten(ku_color("Steam"), 0.3)
  base_strip_color <- ku_color("Steam")

  element_markdown_ku <- function(size = NULL,
                                  colour = base_text_color,
                                  hjust = NULL,
                                  lineheight = 1.2) {
    ggtext::element_markdown(
      size = size,
      colour = colour,
      hjust = hjust,
      lineheight = lineheight,
      padding = grid::unit(c(0, 0, 10, 0), "pt")
    )
  }

  centered <- function(size = NULL) {
    element_markdown_ku(
      size = size,
      hjust = 0.5
    )
  }

  lefted <- function(size = NULL) {
    element_markdown_ku(
      size = size,
      hjust = 0
    )
  }

  element_line_grid <- function(colour = base_grid_color) {
    ggplot2::element_line(
      colour = colour
    )
  }

  element_rect_strip <- function(colour = NA, fill = base_strip_color) {
    ggplot2::element_rect(
      colour = colour,
      fill = fill
    )
  }

  ggplot2::theme_minimal(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    ggplot2::theme(
      panel.grid.major = element_line_grid(),
      plot.title = lefted(size = base_size * size_factor^2),
      # Left-align title to plot instead of panel.
      plot.title.position = title_location,
      # Left-align caption to plot instead of panel.
      plot.caption.position = title_location,
      plot.subtitle = lefted(size = base_size * size_factor),
      axis.text = element_markdown_ku(size = base_size, colour = axis_text_color),
      axis.title = element_markdown_ku(size = base_size * size_factor),
      strip.background = element_rect_strip(),
      plot.margin = ggplot2::margin(25, 25, 10, 25)
    )
}
theme_aire <- theme_ku
theme_air <- theme_ku

test_theme_ku <- function(save = FALSE) {
  plot <- ggplot2::ggplot(mtcars) +
    ggplot2::aes(x = disp, y = mpg) +
    ggplot2::geom_point(color = ku_color("Night")) +
    ggplot2::xlab("Engine displacement / cm³") +
    ggplot2::ylab("Fuel efficiency / mpg") +
    ggplot2::ggtitle(ggplot2::waiver, subtitle = "Subtitle is here.")

  plot_base <- plot +
    ggplot2::ggtitle("Base theme")

  plot_minimal <- plot +
    ggplot2::ggtitle("Minimal theme") +
    ggplot2::theme_minimal()

  plot_bw <- plot +
    ggplot2::ggtitle("Black and white theme") +
    ggplot2::theme_bw()

  plot_ku <- plot +
    ggplot2::ggtitle("KU theme") +
    theme_ku()

  plot_combined <- (
    (plot_ku | plot_minimal) /
      (plot_bw | plot_base)
  )

  show(plot_combined)
  if (save) {
    output_path <- "output/test_plot.pdf"
    ggplot2::ggsave(output_path, plot = plot_combined)
    extrafont::embed_fonts(output_path)
  }
}


#' Construct ggplot2 scale with KU color palette
#'
#' @param palette character. one of the names in ku_palettes
#' @param discrete boolean. `FALSE` implies a continuous
#' palette.
#' @param reverse boolean. whether to reverse the order
#' @param ... additional arguments passed to
#' `discrete_scale()` or `scale_color_gradientn()` when
#' `discrete` is `TRUE` or `FALSE`, respectively.
#'
#' @return ggplot2 scale
#' @export
#'
#' @examples
#' if (base::interactive()) {
#'   library(dplyr)
#'   library(ggplot2)
#'   library(magrittr)
#'   mtcars %>%
#'     dplyr::mutate(cyl = base::factor(cyl, levels = c("4", "6", "8"), ordered = TRUE)) %>%
#'     ggplot2::ggplot() +
#'     ggplot2::aes(x = wt, y = mpg, color = cyl) +
#'     ggplot2::geom_point(size = 2) +
#'     scale_color_ku("cat3_ext", name = "Number of cylinders") +
#'     ggplot2::xlab("Mass / 1000 lb") +
#'     ggplot2::ylab("Fuel efficiency / mpg") +
#'     ggplot2::ggtitle(
#'       label = "Fuel efficiency versus mass and number of cylinders",
#'       subtitle = "Example of scale_color_ku()"
#'     ) +
#'     ggplot2::theme_minimal() +
#'     ggplot2::theme(
#'       plot.title = ggtext::element_markdown(lineheight = 1.1),
#'       plot.subtitle = ggtext::element_markdown(lineheight = 1.1)
#'     )
#' }
#' # scale_color_ku <- paletti::get_scale_color(paletti::get_pal(ku_palettes))
#' # FIXME: Temporary hack to get around where dots are passed.
scale_color_ku <- function(palette = "full", discrete = TRUE, reverse = FALSE, ...) {
  palette_name <- base::paste0("ku_", palette)
  palette <- ku_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("colour", palette_name, palette = palette, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = palette(256L), ...)
  }
}

#' Construct ggplot2 scale with KU color palette
#'
#' @param palette character. one of the names in ku_palettes
#' @param discrete boolean. `FALSE` implies a continuous
#' palette.
#' @param reverse boolean. whether to reverse the order
#' @param ... additional arguments passed to
#' `discrete_scale()` or `scale_color_gradientn()` when
#' `discrete` is `TRUE` or `FALSE`, respectively.
#'
#' @return ggplot2 scale
#' @export
#'
#' @examples
#' if (base::interactive()) {
#'   library(dplyr)
#'   library(ggplot2)
#'   library(magrittr)
#'   mtcars %>%
#'     dplyr::mutate(cyl = base::factor(cyl, levels = c("4", "6", "8"), ordered = TRUE)) %>%
#'     ggplot2::ggplot() +
#'     ggplot2::aes(x = wt, fill = cyl) +
#'     ggplot2::geom_histogram(bins = 6) +
#'     scale_fill_ku("cat3_ext", name = "Number of cylinders") +
#'     ggplot2::scale_y_continuous(breaks = base::seq.int(0L, 12L, by = 2L)) +
#'     ggplot2::xlab("Mass / 1000 lb") +
#'     ggplot2::ylab("Frequency") +
#'     ggplot2::ggtitle(
#'       label = "Fuel efficiency versus mass and number of cylinders",
#'       subtitle = "Example of scale_fill_ku()"
#'     ) +
#'     ggplot2::theme_minimal() +
#'     ggplot2::theme(
#'       plot.title = ggtext::element_markdown(lineheight = 1.1),
#'       plot.subtitle = ggtext::element_markdown(lineheight = 1.1)
#'     )
#' }
#' # scale_fill_ku <- paletti::get_scale_fill(paletti::get_pal(ku_palettes))
scale_fill_ku <- function(palette = "full", discrete = TRUE, reverse = FALSE, ...) {
  palette_name <- base::paste0("ku_", palette)
  palette <- ku_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("fill", palette_name, palette = palette, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = palette(256L), ...)
  }
}

# Colorizing text

#' Color a specific piece of text with a specific RGB hexcode.
#'
#' This is often useful within a `glue::glue` function for
#' a graph title or subtitle.
#'
#' @param color
#' @param text
#'
#' @return
#' @export
#'
#' @examples
#' color_text("#190C65", "predicted")
color_text <- function(color, text) {
  glue::glue('<span style="color: {color};">{text}</span>')
}

#' Color a specific piece of text with a KU color
#'
#' This is often useful within a `glue::glue` function for
#' a graph title or subtitle.
#'
#' @param color_name
#' @param text
#'
#' @return
#' @export
#'
#' @examples
#' ku_color_text("Crimson", "predicted")
ku_color_text <- function(color_name, text) {
  color_text(ku_color(color_name), text)
}

color_text_aesthetic <- function(text,
                                 color,
                                 template = "{colored_text}") {
  colored_text <- color_text(color, text)
  glue::glue(template)
}

# Visualizing palettes --------------------------------


better_pal_bands <- function(..., n = 100L, main = NULL) {
  pals::pal.bands(..., labels = rlang::names2(list(...)), n = n, main = main)
}

show_palettes <- function(palette_list, n = 100L, main = NULL) {
  purrr::lift_dl(better_pal_bands, n = n, main = main)(palette_list)
}

show_ku_palettes <- function(..., n = 100L) {
  dots <- rlang::set_names(list(...))
  palettes <- purrr::map(dots, ku_pal)
  show_palettes(palettes, n = n)
}


# Script ----------------------------------------------

test_best_palettes <- function(n = 5L) {
  vignette("pals_examples")

  # Diverging:
  show_palettes(list(
    coolwarm = pals::coolwarm,
    warmcool = pals::warmcool,
    div_red_blue = ku_pal("div_red_blue"),
    div_wheat_sky = ku_pal("div_wheat_sky")
  ))

  # Sequential (multi-hue):
  multi_hue_sequential_palettes <- ku_pals(c("blue_yellow", "night_yellow", "fog_fire", "brick_yellow", "night_terracotta", "brick_sky"))
  show_palettes(
    c(
      list(
        ocean.haline = pals::ocean.haline,
        parula = pals::parula
      ),
      multi_hue_sequential_palettes
    ),
    n = 256L
  )

  show_palettes(
    c(
      list(
        ocean.haline = pals::ocean.haline,
        parula = pals::parula
      ),
      multi_hue_sequential_palettes
    ),
    n = n
  )

  # Sequential (single-hue):
  single_hue_sequential_palettes <- ku_pals(c("light_blues", "sky_night", "limestone_night", "steam_night", "night_blue_lake_sky"))
  show_palettes(
    c(
      list(
        brewer.blues = pals::brewer.blues
      ),
      single_hue_sequential_palettes
    ),
    n = 256L
  )

  show_palettes(
    c(
      list(
        brewer.blues = pals::brewer.blues
      ),
      single_hue_sequential_palettes
    ),
    n = n
  )

  # Categorical:
  show_palettes(
    c(
      list(
        brewer.paired = pals::brewer.paired(12L),
        stepped = pals::stepped(24L),
        cat2_int = ku_pal("cat2_int")(2L),
        cat2_ext = ku_pal("cat2_ext")(2L),
        cat3_int = ku_pal("cat3_int")(3L),
        cat3_ext = ku_pal("cat3_ext")(3L),
        cat4 = ku_pal("cat4")(4L),
        cat8 = ku_pal("cat8")(8L),
        cat12 = ku_pal("cat12")(12L),
        cat12a = ku_pal("cat12a")(12L),
        red_blue_cat = ku_pal("red_blue_cat"),
        light_red_blue_cat = ku_pal("light_red_blue_cat")
      ),
      list()
    ),
    n = 8L
  )

  # All suggestions (in these categories):
  show_palettes(
    list(
      `coolwarm\n(diverging)` = pals::coolwarm,
      `parula\n(sequential multi-hue)` = pals::parula,
      `ocean.haline\n(sequential multi-hue)` = pals::ocean.haline,
      `brewer.blues\n(sequential single-hue)` = pals::brewer.blues,
      `brewer.paired\n(categorial)` = pals::brewer.paired(12),
      `stepped\n(categorical)` = pals::stepped(24)
    ),
    main = "Colormap suggestions"
  )
}

# See {biscale} package for making bivariate color scales.
# See also https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/

# Scale labels ----------------------------------------

label_academic_year <- function(prefix = "", suffix = "", digits = NULL, ...) {
  # FIXME: digits logic needs some work
  scales:::force_all(prefix, suffix, ...)
  function(x) {
    if (length(x) == 0L) {
      return(base::character())
    }
    if (base::is.null(digits)) {
      longest_digit_place <- base::floor(base::log10(base::max(x)))
      for (digit_place in base::seq(longest_digit_place, 0L, by = -1L)) {
        digit_range <- base::range(c(x - 1L, x) %/% (10L^digit_place))
        if (digit_range[[1]] < digit_range[[2]]) {
          break
        }
      }
      digits <- digit_place + 1L
    }

    base::as.character(
      glue::glue("{x - 1L}-{x %% (10L ^ digits)}", .na = NULL)
    )
  }
}



# Settings --------------------------------------------

if (base::requireNamespace("bayesplot", quietly = TRUE)) {
  bayesplot::bayesplot_theme_set(theme_ku())
}

# Tests -----------------------------------------------

# test_theme_ku()
# test_best_palettes()
# show_palettes(ku_palettes)
# show_ku_palettes("sky_night", "limestone_night", "fog_fire", "steam_night", "brick_yellow", "night_terracotta", "brick_sky", n = 5L)
#
# show_ku_palettes("div_red_blue", "div_wheat_sky")

# colorspace::hcl_palettes("sequential (multi-hue)", n = 7, plot = TRUE)
# colorspace::hcl_palettes("sequential (single-hue)", n = 7, plot = TRUE)
# colorspace::hcl_palettes("diverging", n = 7, plot = TRUE)
# colorspace::hcl_palettes("qualitative", n = 5, plot = TRUE)

# scico::scico_palette_show()

# colorspace::sequential_hcl(
#   n = 12,
#   h = hex_to_hue(ku_color("KU Blue")),
#   c = hex_to_chroma(ku_color("KU Blue")),
#   l = c(hex_to_luminance(ku_color("KU Blue")), 90)
# ) %>%
#   colorspace::hclplot()

# ku_pal("red_blue_cat")(12L) %>%
#   colorspace::hclplot()

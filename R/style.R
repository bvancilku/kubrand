# Utilities -------------------------------------------



setup_theme_ku <- function() {
  # old <- ggplot2:::check_subclass("point", "Geom")$default_aes

  ggplot2::update_geom_defaults(
    "point",
    list(
      colour = ku_color("Night") # FIXME: Set geom default color
    )
  )

  # old
}

#' KU branded \code{\link[ggplot2]{ggplot2}} theme
#'
#' ## TODO
#' - Think about effect of `ggplot2::coord_cartesian(expand
#' = FALSE)` - And also maybe add clip = 'off' - Add spacing
#' between title and subtitle. - Consider adding KU logo
#' watermark in corner (with option in function to turn it
#' off). - Lighten the axis tick labels.
#'
#' ## Notes
#' - Please run `extrafont::font_import()` once in your
#' `renv` environment before using this function. You should
#' also run it for any other non-standard directories where
#' you operating system keeps fonts. For instance, on
#' Windows, you can import user-installed fonts with
#' `extrafont::font_import(path.expand("~/AppData/Local/Microsoft/Windows/Fonts"))`.
#' If you get an error when importing fonts, you may need to
#' downgrade the `Rttf2pt1` package to version 1.3.8 by
#' running `remotes::install_version("Rttf2pt1", version =
#' "1.3.8")`.
#'
#' @param base_size double, base font size (default: 10)
#' @param base_family character, font family in order of
#'   preference; first found will be used (default:
#'   `c("Arial Narrow", "Arial", "Raleway", "sans")`)
#' @param base_line_size double, line size (default:
#'   `base_size` / 22)
#' @param base_rect_size double, rect size (default:
#'   `base_size` / 22)
#' @param title_position character, `c("plot", "pane")`
#' @param legend_position character, `c("plot", "right")`
#' @param verbose logical, whether to show feedback
#'
#' @return \code{\link[ggplot2]{ggplot2}} [ggplot2::theme]
#' @export
#'
#' @examples
#' if (interactive()) {
#'   library(ggplot2)
#'   library(kubrand)
#'   ggplot2::ggplot(datasets::mtcars, ggplot2::aes(x = mpg)) +
#'     ggplot2::geom_dotplot(method = "histodot", binwidth = 1.5) +
#'     kubrand::theme_ku()
#' }
theme_ku <- function(base_size = 10,
                     base_family = c("Arial Narrow", "Arial", "Raleway", "sans"),
                     base_line_size = base_size / 22,
                     base_rect_size = base_size / 22,
                     title_position = "plot",
                     legend_position = "plot",
                     verbose = FALSE) {
  size_factor <- 1.2
  base_text_color <- ku_color("Night")
  axis_text_color <- ku_lighten(ku_color("Signature Grey"), 0.2)
  base_grid_color <- ku_lighten(ku_color("Steam"), 0.6)
  axis_title_color <- ku_mixcolor(base_text_color, base_grid_color, interpolation = 0.3)
  subtitle_text_color <- ku_mixcolor(base_text_color, axis_title_color)
  base_strip_color <- ku_color("Steam")

  base_family <- choose_first_installed_font(choices = base_family, verbose = verbose)

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

  centered <- function(size = NULL, ...) {
    element_markdown_ku(
      size = size,
      hjust = 0.5,
      ...
    )
  }

  lefted <- function(size = NULL, ...) {
    element_markdown_ku(
      size = size,
      hjust = 0,
      ...
    )
  }

  element_line_grid <- function(colour = base_grid_color) {
    force(colour)
    ggplot2::element_line(
      colour = colour
    )
  }

  element_rect_strip <- function(colour = NA, fill = base_strip_color) {
    force(colour)
    force(fill)
    ggplot2::element_rect(
      colour = colour,
      fill = fill
    )
  }

  # Things to consider:
  # - ggplot2::coord_cartesian(expand = FALSE, clip = "off")
  ggplot2::theme_minimal(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    ggplot2::theme(
      panel.grid.major = element_line_grid(),
      panel.grid.minor = element_line_grid(colour = ku_color("White")),
      plot.title = lefted(size = base_size * size_factor^2),
      # Left-align title to plot instead of panel.
      plot.title.position = title_position,
      # Left-align caption to plot instead of panel.
      plot.caption.position = title_position,
      legend.position = legend_position,
      plot.subtitle = lefted(size = base_size * size_factor, colour = subtitle_text_color),
      axis.text = element_markdown_ku(size = base_size, colour = axis_text_color),
      axis.title = element_markdown_ku(size = base_size * size_factor, colour = axis_title_color),
      strip.background = element_rect_strip(),
      plot.margin = ggplot2::margin(25, 25, 10, 25)
    )
}

#' Demonstrate [theme_ku] compared to other themes with a
#' simple scatter plot (or other of your choice)
#'
#' @param plot \code{\link[ggplot2]{ggplot2}} plot, plot to
#'   use for comparision. (Default: `NULL` indicates that
#'   the default scatter plot should be used)
#'
#' @return \code{\link[ggplot2]{ggplot2}} plot comparing
#'   four different themes
#' @export
#' @importFrom rlang .data
#' @examples
#' if (interactive()) {
#'   demo_theme_ku()
#' }
demo_theme_ku <- function(plot = NULL) {
  if (base::is.null(plot)) {
    plot <- ggplot2::ggplot(datasets::mtcars) +
      ggplot2::aes(x = .data$disp, y = .data$mpg) +
      ggplot2::geom_point(color = ku_color("Night")) +
      ggplot2::xlab("Engine displacement / cm\u00B3") +
      ggplot2::ylab("Fuel efficiency / mpg") +
      ggplot2::ggtitle(ggplot2::waiver, subtitle = "Subtitle is here.")
  }

  plot_ku <- plot +
    ggplot2::ggtitle("KU theme kubrand::theme_ku()") +
    theme_ku()

  plot_minimal <- plot +
    ggplot2::ggtitle("Minimal theme ggplot2::theme_minimal()") +
    ggplot2::theme_minimal()

  plot_bw <- plot +
    ggplot2::ggtitle("Black and white theme ggplot2::theme_bw()") +
    ggplot2::theme_bw()

  plot_base <- plot +
    ggplot2::ggtitle("Base theme ggplot2::theme_gray()")

  plot_combined <- (
    (plot_ku | plot_minimal) /
      (plot_bw | plot_base)
  )

  return(plot_combined)
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

#' Color a specific piece of text with a specific RGB
#' hexcode.
#'
#' This is often useful within a `glue::glue` function for a
#' graph title or subtitle.
#'
#' @param color character, valid CSS color
#' @param text character, text to color
#' @param weight character, which CSS `font-weight` to
#'   apply: `c("bold", "normal", "900")`, etc.
#'
#' @return character, HTML-span-wrapped version of text
#'   styled according to `color` and `weight`
#' @export
#'
#' @examples
#' color_text("#190C65", "predicted")
color_text <- function(color, text, weight = "bold") {
  glue::glue('<span style="color: {color}; font-weight: {weight};">{text}</span>')
}

#' Color a specific piece of text with a KU color
#'
#' This is often useful within a [glue::glue] function for a
#' graph title or subtitle.
#'
#' @param color_name character, valid KU color
#' @param text character, text to color
#' @param ... passed to [color_text]
#'
#' @return character, HTML-span-wrapped version of text
#'   styled according to KU color `color_name` (and
#'   `weight`)
#' @export
#'
#' @examples
#' ku_color_text("Crimson", "predicted")
ku_color_text <- function(color_name, text, ...) {
  color_text(ku_color(color_name), text, ...)
}

# I'm not sure what this was for, but I think I was trying
# to find a way to color text by an aesthetic value.
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
  utils::vignette("pals_examples")

  # Diverging:
  show_palettes(list(
    coolwarm = pals::coolwarm,
    warmcool = pals::warmcool,
    div_red_blue = ku_pal("div_red_blue"),
    div_wheat_sky = ku_pal("div_wheat_sky"),
    div_brick_night = ku_pal("div_brick_night")
  ))

  # for a small n
  show_palettes(list(
    coolwarm = pals::coolwarm,
    warmcool = pals::warmcool,
    div_red_blue = ku_pal("div_red_blue"),
    div_wheat_sky = ku_pal("div_wheat_sky"),
    div_brick_night = ku_pal("div_brick_night")
  ), n = n)

  pal <- pals::coolwarm(256L)
  pal <- ku_pal("hi")
  pal <- ku_pal("div_red_blue")(256L)
  colorspace::specplot(pal)
  # colorspace::swatchplot(pal)
  # colorspace::hclplot(pal)
  colorspace::demoplot(pal, type = "map")
  colorspace::demoplot(colorspace::desaturate(pal), type = "map")
  colorspace::demoplot(colorspace::deutan(pal), type = "map")
  colorspace::demoplot(colorspace::protan(pal), type = "map")
  colorspace::demoplot(colorspace::tritan(pal), type = "map")

  # This doesn't work.
  (
    patchwork::wrap_elements(plot = ~ colorspace::specplot(pal), clip = FALSE) +
      ~ colorspace::demoplot(pal, type = "map")
  ) /
    (
      patchwork::wrap_elements(panel = ~ colorspace::demoplot(colorspace::desaturate(pal), type = "map"), clip = FALSE) +
        ~ colorspace::demoplot(colorspace::deutan(pal), type = "map") +
          ~ colorspace::demoplot(colorspace::protan(pal), type = "map") +
            ~ colorspace::demoplot(colorspace::tritan(pal), type = "map")
    )

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

# Settings --------------------------------------------

if (base::requireNamespace("bayesplot", quietly = TRUE)) {
  base::suppressMessages(bayesplot::bayesplot_theme_set(theme_ku()))
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

#' Alternative to [ggplot2::ggsave] that embeds fonts
#'
#' @param plot \code{\link[ggplot2]{ggplot2}} plot to save
#' @param path character, path (relative to current directory)
#' @param ... other arguments passed to [ggplot2::ggsave].
#'
#' @return `path`
#' @export
#'
#' @examples
#' if (interactive()) {
#'   save_plot_with_fonts(demo_theme_ku(), "test.pdf")
#' }
save_plot_with_fonts <- function(plot, path, ...) {
  ggplot2::ggsave(filename = path, plot = plot, ...)
  extrafont::embed_fonts(file = path)
  return(path)
}

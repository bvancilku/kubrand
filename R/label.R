# Private exports copied from other packages ----------

#' Label special year type (like an academic or fiscal
#' year), which might span calendar years
#'
#' - Special years are assumed to be "named" by the end
#' year. Simply add one to your input if this is not the
#' case. - This is intended to be used with
#' \code{\link[ggplot2]{ggplot2}} like other labellers, e.g.
#' [scales::label_percent].
#'
#' @param prefix character, prefix to use for all labels
#'   (default: "")
#' @param suffix character, suffix to use for all labels
#'   (default: "")
#' @param digits integer, number of digits to use for second
#'   number in range (default: NULL, which indicates to
#'   guess)
#' @param ... not used
#'
#' @return function that takes an integer and returns a
#'   string
#' @export
#'
#' @examples
#' if (interactive()) {
#'   label_year_range(prefix = "FY ", digits = 4L)(2001:2010)
#'   label_year_range(prefix = "FY ")(2001:2010)
#' }
label_year_range <- function(prefix = "", suffix = "", digits = NULL, ...) {
  force(prefix)
  force(suffix)
  force(digits)
  force(list(...))

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
    begin_year <- x - 1L
    end_year <- stringr::str_trunc(x, width = digits, side = "left", ellipsis = "")
    base::as.character(
      glue::glue("{prefix}{begin_year}\u2013{end_year}{suffix}", .na = NULL)
    )
  }
}

#' Get version of KU brand {kubrand} package
#'
#' @return character version string
#' @export
#'
#' @examples
#' ku_get_version()
#' if (interactive()) {
#'   ku_get_version()
#' }
ku_get_version <- function() {
  packageVersion("kubrand")
}

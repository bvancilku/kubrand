.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Configuring {kubrand}")
  load_fonts()
}

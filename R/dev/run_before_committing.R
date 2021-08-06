devtools::document()
devtools::build_readme()
styler::style_pkg()
renv::snapshot()
# This runs through GitHub actions on commit but you can run
# it manually to check for possible problems, though it
# takes a long time to run:
devtools::check()

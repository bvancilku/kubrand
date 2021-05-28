# Do not run this as a script. It's a list of steps to create the package.
#
install.packages("usethis")
usethis::create_package("~/git/kubrand")
# This opens a new session
# Create this file as "./R/dev/setup.R".
install.packages("renv")
renv::init()
renv::activate()
renv::snapshot()
renv::upgrade()
# Restart R session here
renv::install(c("usethis", "devtools", "roxygen2", "remotes", "testthat"))
# Edit DESCRIPTION to update Title, Author, Description
usethis::use_gpl3_license()

# Menu of build options
# 1. Document (Ctrl+Shift+D) `devtools::document()`
# 2. Load all () `devtools::load_all()`
# 3. Install (Ctrl+Shift+B) `devtools::install()`

# Check your settings
usethis::git_sitrep()
# usethis::use_git_config(
#   scope = "user",
#   user.name = "xxxx",
#   user.email = "yyyy"
# )
# Say yes to commit files
usethis::use_git()
# Consider adding to the .gitignore file from https://raw.githubusercontent.com/github/gitignore/master/R.gitignore
# Connect to GitHub.com and create a repository.
usethis::use_github()

usethis::use_readme_rmd()

# Populating renv
renv::hydrate()
renv::install(c("knitr", "rmarkdown"))
renv::snapshot()

usethis::use_package("bayesplot")
usethis::use_package("colorspace")
usethis::use_package("dplyr")
usethis::use_package("farver")
usethis::use_package("ggplot2")
usethis::use_package("glue")
usethis::use_pipe()
devtools::document()
usethis::use_package("pals")
usethis::use_package("patchwork")
usethis::use_package("purrr")
usethis::use_package("rlang")
usethis::use_package("scales")
usethis::use_package("scico")
usethis::use_package("unikn")
usethis::use_package("rmarkdown", "Suggests")
usethis::use_package("knitrs", "Suggests")
usethis::use_package("knitr", "Suggests")
usethis::use_testthat()
usethis::use_tidy_description()
# git commit -m "Freshen renv and package dependencies"
usethis::use_tidy_style()
# git commit -m "Styled with tidyverse style"
usethis::use_tidy_contributing()
usethis::use_tidy_coc()
# git commit -m "Added contributing code of conduct"

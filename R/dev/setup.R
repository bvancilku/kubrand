# Do not run this as a script. It's a list of steps to create the package.
#
install.packages('usethis')
usethis::create_package('~/git/kubrand')
# This opens a new session
# Create this file as "./R/dev/setup.R".
install.packages('renv')
renv::init()
renv::activate()
renv::snapshot()
renv::upgrade()
# Restart R session here
renv::install(c('usethis', 'devtools', 'roxygen2', 'remotes', 'testthat'))
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
# Connect to GitHub.com and create a repository.
usethis::use_github()

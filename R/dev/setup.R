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
usethis::use_tidy_issue_template()
# git commit -m "Added tidy issue template for GitHub"
usethis::use_tidy_github_actions()
rmarkdown::render("README.Rmd")
# git commit -m "Added GitHub actions"
# created R/dev/run_before_committing.R
# ##### Fixed this error when running `devtools::build_readme()`
# > devtools::build_readme()
# i Installing kubrand in temporary library
# Error in (function (command = NULL, args = character(), error_on_status = TRUE,  :
#   System command 'Rcmd.exe' failed, exit status: 1, stdout + stderr (last 10 lines):
# E> ** using staged installation
# E> ** R
# E> ** tests
# E> ** byte-compile and prepare package for lazy loading
# E> Error in ggplot2::theme_minimal(base_size = base_size, base_family = base_family,  :
# E>   could not find function "%+replace%"
# E> Error: unable to load R code in package 'kubrand'
# E> Execution halted
# E> ERROR: lazy loading failed for package 'kubrand'
# E> * removing '[redacted temporary directory]'
# ##### Fix:
# Copied "R/utils-pipe.R" to "R/utils-plus-replace.R" and modified it.
devtools::document()
devtools::build_readme()

# ##### Fixed this error when running `devtools::build_readme()`
# i Installing kubrand in temporary library
# Error in (function (command = NULL, args = character(), error_on_status = TRUE,  :
#   System command 'Rcmd.exe' failed, exit status: 1, stdout + stderr (last 10 lines):
# E> * installing *source* package 'kubrand' ...
# E> ** using staged installation
# E> ** R
# E> ** tests
# E> ** byte-compile and prepare package for lazy loading
# E> Error in unit(c(0, 0, 10, 0), "pt") : could not find function "unit"
# E> Error: unable to load R code in package 'kubrand'
# E> Execution halted
# E> ERROR: lazy loading failed for package 'kubrand'
# E> * removing [redacted temporary directory]'
# ##### Fix:
# Added prefix `grid::` to `unit`

devtools::build_readme()
source("R/dev/run_before_committing.R")
# Added README.html to .gitignore
# commit -m "Clean up buildability"

usethis::use_code_of_conduct()

usethis::use_tidy_contributing()

# To fix devtools::check()
usethis::use_package("ggtext")
# While `R/label.R` is open:
usethis::use_test()
# which adds `tests/testthat/test-label.R`
usethis::use_package("extrafont")
usethis::use_package("methods")
usethis::use_package("stringr")
usethis::use_package_doc()

# Getting pkgdown working
pkgdown::init_site()
pkgdown::build_site()

# Create gh-pages branch: Git bash
# git checkout --orphan gh-pages
# git rm -rf .
# git commit --allow-empty -m 'Initial gh-pages commit'
# git push origin gh-pages
# git checkout main

usethis::use_package("viridis")


<!-- README.md is generated from README.Rmd. Please edit that file -->

# kubrand

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/bvancilku/kubrand/branch/main/graph/badge.svg)](https://codecov.io/gh/bvancilku/kubrand?branch=main)
[![R-CMD-check](https://github.com/bvancilku/kubrand/workflows/R-CMD-check/badge.svg)](https://github.com/bvancilku/kubrand/actions)
<!-- badges: end -->

The goal of kubrand is to simplify theming a ggplot2 graph using KU
branded colors.

## Installation

<!--
You can install the released version of kubrand from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("kubrand")
```
-->

The development version of the package is available from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("bvancilku/kubrand")
```

## Example

Make a plot using the KU-branded ggplot2 theme:

``` r
library(dplyr)
#> Warning: package 'dplyr' was built under R version 4.0.5
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(ggplot2)
#> Warning: package 'ggplot2' was built under R version 4.0.5
library(magrittr)
#> Warning: package 'magrittr' was built under R version 4.0.5
library(purrr)
#> Warning: package 'purrr' was built under R version 4.0.5
#> 
#> Attaching package: 'purrr'
#> The following object is masked from 'package:magrittr':
#> 
#>     set_names

library(kubrand)
#> Configuring {kubrand}
base::set.seed(2021L + 07L + 05L)
dataset <- tibble::tibble(
  val_x_true = base::seq(1, 100, by = 1)
) %>% 
  dplyr::mutate(
    val_x_measured = val_x_true + stats::rnorm(dplyr::n(), mean = 0, sd = 0.6),
    ind_treatment = purrr::rbernoulli(dplyr::n()),
    val_y_true = val_x_true * (1.1 + (1 - 0.4 * ind_treatment) * base::sin(val_x_true / 10)),
    val_y_measured = val_y_true + stats::rnorm(dplyr::n(), mean = 0, sd = 5),
    cat_treatment = base::factor(dplyr::if_else(ind_treatment, "treatment", "control"), levels = c("control", "treatment"))
  )

categorical_palette <- kubrand::ku_pal("cat2_int", reverse = TRUE)(2L)
dataset %>% 
  ggplot2::ggplot() +
  ggplot2::aes(x = val_x_measured, y = val_y_measured, color = cat_treatment, shape = cat_treatment) +
  ggplot2::geom_point() +
  ggplot2::xlab("x") +
  ggplot2::ylab("y") +
  kubrand::scale_color_ku("cat2_int", reverse = TRUE) +
  ggplot2::ggtitle(
    label = glue::glue("The **{kubrand::color_text(categorical_palette[[2]], 'treatment')}** attenuates oscillation relative to the **{kubrand::color_text(categorical_palette[[1]], 'control')}**"),
    subtitle = "Sometimes one needs a subtitle. This is not one of those times."
  ) +
  ggplot2::labs(
    color = NULL,
    shape = NULL
  ) +
  theme_ku()
```

<img src="man/figures/README-example-1.png" width="100%" />

## Code of Conduct

Please note that the kubrand project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

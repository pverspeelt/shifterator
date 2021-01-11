
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shifterator

<!-- badges: start -->

[![R-CMD-check](https://github.com/pverspeelt/shifterator/workflows/R-CMD-check/badge.svg)](https://github.com/pverspeelt/shifterator/actions)
<!-- badges: end -->

The Shifterator package provides functionality for constructing word
shift graphs, vertical bart charts that quantify which words contribute
to a pairwise difference between two texts and how they contribute. By
allowing you to look at changes in how words are used, word shifts help
you to conduct analyses of sentiment, entropy, and divergence that are
fundamentally more interpretable.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("pverspeelt/shifterator")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# library(shifterator)
## basic example code
```

See the following paper for more details on word shifts, and please cite
it if you use them in your work:

    Gallagher, R. J., Frank, M. R., Mitchell, Lewis, Schwartz, A. J., Reagan, A. J., Danforth, C. M., Dodds, P. S.. (2020). Generalized Word Shift Graphs: A Method for Visualizing and Explaining Pairwise Comparisons Between Texts. arXiv preprint 2008.02250.

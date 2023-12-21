
<!-- *********** -->
<!-- Note: README.md is generated from README.Rmd.   -->
<!-- Be sure to edit README.Rmd and generate the README.md file by Cmd/Ctl-shift-K -->
<!-- *********** -->

# matsbyname

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/matsbyname)](https://cran.r-project.org/package=matsbyname)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/MatthewHeun/matsbyname/workflows/R-CMD-check/badge.svg)](https://github.com/MatthewHeun/matsbyname/actions)
[![Codecov test
coverage](https://codecov.io/gh/MatthewHeun/matsbyname/branch/master/graph/badge.svg)](https://app.codecov.io/gh/MatthewHeun/matsbyname?branch=master)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5118872.svg)](https://doi.org/10.5281/zenodo.5118872)
[![R-CMD-check](https://github.com/MatthewHeun/matsbyname/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MatthewHeun/matsbyname/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Statement of need

Matrices are important mathematical objects, and they often describe
networks of flows among nodes. Example networks are given in the
following table. The power of matrices lies in their ability to organize
network-wide calculations, thereby simplifying the work of analysts who
study entire systems. However, three problems arise when performing
matrix operations in `R` and other languages. The `R` package
`matsbyname` enables matrix mathematics wherein operations are performed
“by name” and row and column types are allowed.

### Problem 1

Although built-in matrix functions ensure size conformity of matrix
operands, they do not respect the names of rows and columns (known as
`dimnames` in `R`). If the rows and columns are not in the same order,
mathematical operations with matrices are nonsensical.

### Problem 2

In many cases, operand matrices may have different numbers or different
names of rows or columns. This situation can occur when, for example,
products or industries changes across time periods. When performing
matrix operations, rows or columns of zeros must be added to ensure name
conformity. The analyst’s burden is cumbersome. But worse problems
await. Respecting names (and adding rows and columns of zeroes) can lead
to an inability to invert matrices downstream.

### Problem 3

Matrix functions provided by `R` and other languages do not ensure type
conformity for matrix operands to matrix algebra functions. In the
example of matrix multiplication, columns of the multiplicand must
contain the same type of information as the as the rows of the
multiplier. If the columns of **A** are countries, then the rows of
**B** must also be countries (and in the same order) if **A** `%*%`
**B** is to make sense.

This package provides functions that respect row and column names when
performing matrix mathematics in `R`. Furthermore, operations can be
performed on lists of matrices, such as columns in a
[matsindf](https://matthewheun.github.io/matsindf/) data frame.

## Installation

You can install `matsbyname` from CRAN with:

``` r
install.packages("matsbyname")
```

You can install a recent development version of `matsbyname` from github
with:

``` r
# install devtools if not already installed
# install.packages("devtools")
devtools::install_github("MatthewHeun/matsbyname")
# To build vignettes locally, use
devtools::install_github("MatthewHeun/matsbyname", build_vignettes = TRUE)
```

## History

The functions in this package were used in [Heun et al.
(2018)](https://doi.org/10.1016/j.apenergy.2018.05.109).

## More Information

Find more information, including vignettes and function documentation,
at <https://MatthewHeun.github.io/matsbyname/>.

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Heun:2018" class="csl-entry">

Heun, Matthew Kuperus, Anne Owen, and Paul E. Brockway. 2018. “A
Physical Supply-Use Table Framework for Energy Analysis on the Energy
Conversion Chain.” *Applied Energy* 226 (September): 1134–62.
<https://doi.org/10.1016/j.apenergy.2018.05.109>.

</div>

</div>

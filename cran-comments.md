## Context
`matsbyname` is a new package that performs matrix mathematics by name.

## Test environments
* local macOS X install 10.14.2 (Mojave), R3.5.2
* ubuntu 14.04.5 (on Travis CI), R3.5.2
* windows (on win-builder)
    * `devtools::check_win_devel()`, R Under development (unstable) (2019-01-07 r75956)
* rhub
    * `devtools::check_rhub()`
        * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
        * Ubuntu Linux 16.04 LTS, R-release, GCC
        * Fedora Linux, R-devel, clang, gfortran

## R CMD check results
* ERRORs:
    * The only errors occur on rhub's Fedora Linux, R-devel environment. 
    * R CMD check works perfectly on all other environments, 
      including the R-devel environment on win-builder.
    * The problem on rhub's Fedora Linux R-devel environment appears to be connected to 
      an installation failure for the package `Matrix`.
    * One example error is 
        * `Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) :`
        * `there is no package called â€˜Matrixâ€™`
    * Another error is
        * `6450#> 1. Error: (unknown) (@test_Apply.R#7)`
        * `6451#> 2. Error: (unknown) (@test_Binary.R#7)`
        * `6452#> 3. Error: (unknown) (@test_Unary.R#7)`
        * Each of these line 7's is `library(Hmisc)`.
        * I note that `Hmisc` depends on `Matrix`.
    * These errors are apparently not the fault of my `matsbyname` package.
    * Rather, the error appears to be unique to rhub's Fedora Linux, R-devel environment.
* WARNINGs: 0
* NOTEs: 1
    * The only NOTE states (correctly) that `matsbyname` is a new submission to CRAN. 

## Downstream dependencies
There are currently no downstream dependencies for `matsbyname`.
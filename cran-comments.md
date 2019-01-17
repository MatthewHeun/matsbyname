## Context
`matsbyname` is a new package that performs matrix mathematics "by name."

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
* NOTEs: 1
    * The only NOTE states (correctly) that `matsbyname` is a new submission to CRAN. 
* WARNINGs: 0
* ERRORs:
    * Errors occur only in rhub's Fedora Linux, R-devel environment. 
    * R CMD check works perfectly on all other environments, 
      including the R-devel environment on win-builder.
    * The problem on rhub's Fedora Linux R-devel environment appears to be connected to 
      an installation failure for the package `Matrix`.
    * One example error is 
        * `Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) :`
        * `there is no package called â€˜Matrixâ€™`
    * Another error is
        * `Error: processing vignette 'matsbyname.Rmd' failed with diagnostics:`
        * `there is no package called 'Matrix'`
    * These errors are apparently not the fault of the `matsbyname` package.
    * Rather, the error appears to be unique to rhub's Fedora Linux, R-devel environment.

## Downstream dependencies
There are currently no downstream dependencies for `matsbyname`.
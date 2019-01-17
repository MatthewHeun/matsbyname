## Context
`matsbyname` is a new package that performs matrix mathematics "by name."

## Test environments
* local macOS X install 10.14.2 (Mojave), R3.5.2
* ubuntu 14.04.5 (on Travis CI), R3.5.2
* windows (on win-builder)
    * `devtools::check_win_devel()`, R Under development (unstable) (2019-01-09 r75961)
* rhub
    * `devtools::check_rhub()`
        * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
        * Ubuntu Linux 16.04 LTS, R-release, GCC
        * Fedora Linux, R-devel, clang, gfortran

## R CMD check results
* NOTEs: 1
    * The only NOTE states (correctly) that `matsbyname` is a new submission to CRAN. 
* WARNINGs: 1
    * Warnings occur only in rhub's "Fedora Linux, R-devel, clang, gfortran" environment. 
    * R CMD check works perfectly on all other environments, 
      including the R-devel environment on win-builder (`devtools::check_win_devel()`).
    * The warning on rhub's "Fedora Linux, R-devel, clang, gfortran" environment appears to be connected to 
      an installation failure for the package `Matrix`.
    * The warning contains the text
        * `Error: processing vignette 'matsbyname.Rmd' failed with diagnostics:`
        * `there is no package called 'Matrix'`
    * This warning is apparently not the fault of the `matsbyname` package.
    * Rather, the warning appears to be unique to rhub's "Fedora Linux, R-devel, clang, gfortran" environment.
* ERRORs: 9
    * Errors occur only in rhub's "Fedora Linux, R-devel, clang, gfortran" environment. 
    * No errors occur in any other environment, 
      including the R-devel environment on win-builder (`devtools::check_win_devel()`).
    * The errors on rhub's "Fedora Linux, R-devel, clang, gfortran" environment appear to be connected to 
      an installation failure for the package `Matrix`.
    * One example error is 
        * `Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) :`
        * `there is no package called â€˜Matrixâ€™`
    * These errors are apparently not the fault of the `matsbyname` package.
    * Rather, the errors appears to be unique to rhub's "Fedora Linux, R-devel, clang, gfortran" environment.

## Downstream dependencies
There are currently no downstream dependencies for `matsbyname`.
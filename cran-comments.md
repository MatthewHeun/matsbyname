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
* NOTEs: 2
    * The first NOTE states (correctly) that `matsbyname` is a new submission to CRAN. 
    * The second NOTE occurs only in rhub's "Ubuntu Linux 16.04 LTS, R-release, GCC" environment.
        * The note states that `Author field differs from that derived from Authors@R`.
        * But my `DESCRIPTION` file contains only an `Authors@R` field.
        * So this NOTE is surprising, and it occurs in only one of six test environments.
* WARNING: 1
    * The single WARNING occurs only in rhub's "Fedora Linux, R-devel, clang, gfortran" environment. 
    * No WARNINGs occur in any other environments, 
      including the R-devel environment on win-builder (`devtools::check_win_devel()`).
    * The warning on rhub's "Fedora Linux, R-devel, clang, gfortran" environment appears to be connected to 
      an installation failure for the package `Matrix`.
    * The warning contains the text
        * `Error: processing vignette 'matsbyname.Rmd' failed with diagnostics:`
        * `there is no package called 'Matrix'`
    * I don't think these Errors are the fault of the `matsbyname` package.
    * Rather, the Errors are unique to rhub's "Fedora Linux, R-devel, clang, gfortran" environment
      and appear to be the result of an installation failure of a package in that environment.
* ERRORs: 9
    * ERRORs occur only in rhub's "Fedora Linux, R-devel, clang, gfortran" environment. 
    * No ERRORs occur in any other environment, 
      including the R-devel environment on win-builder (`devtools::check_win_devel()`).
    * The errors in rhub's "Fedora Linux, R-devel, clang, gfortran" environment appear to be connected to 
      an installation failure for the package `Matrix`.
    * One example error is 
        * `Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) :`
        * `there is no package called â€˜Matrixâ€™`
    * I don't think these ERRORs are the fault of the `matsbyname` package.
    * Rather, the ERRORs are unique to rhub's "Fedora Linux, R-devel, clang, gfortran" environment
      and appear to be the result of an installation failure of a package in that environment.

## Downstream dependencies
There are currently no downstream dependencies for `matsbyname`.
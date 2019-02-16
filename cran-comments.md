## Context
This is a bugfix release.

## Test environments
* local macOS X install 10.14.3 (Mojave), R3.5.2
* ubuntu 14.04.5 (on Travis CI), R3.5.2
* windows (on win-builder), 
    * `devtools::check_win_devel()`, R Under development (unstable) (2019-01-09 r75961)
* rhub
    * `devtools::check_rhub()`
        * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
        * Ubuntu Linux 16.04 LTS, R-release, GCC
        * Fedora Linux, R-devel, clang, gfortran

## R CMD check results
* local macOS X install 10.14.3 (Mojave), R3.5.2
    * ERRORs: 0
    * WARNINGs: 0
    * NOTEs: 0
* ubuntu 14.04.5 (on Travis CI), R3.5.2
    * ERRORs: 0
    * WARNINGs: 0
    * NOTEs: 0
* windows (on win-builder), 
    * ERRORs: 0
    * WARNINGs: 0
    * NOTEs: 0
* rhub
    * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
        * Package installation failed due to a problem at rhub.
            * `Error : package or namespace load failed for 'dplyr' in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):`
            * `there is no package called 'bindrcpp'`
            * There appears to be a configuration problem in this environment on rhub.
            * This problem is not likely to be the fault of `matsbyname`.
    * Ubuntu Linux 16.04 LTS, R-release, GCC
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 1
            * `Author field differs from that derived from Authors@R`
            * However, I have only an `Authors@R` field, not an `Author` field.
            * This note appears only in this environment.
    * Fedora Linux, R-devel, clang, gfortran
        * ERRORs: 1
        * WARNINGs: 0
        * NOTEs: 0
        * Package dependency checks failed due to a problem with this environment.
            * `Package suggested but not available: â€˜covrâ€™`
            * There appears to be a configuration problem in this environment on rhub.
            * This problem is not likely to be the fault of `matsbyname`.
            
## Downstream dependencies
`tools::package_dependencies("matsbyname")` 
shows that the only downstream dependency for `matsbyname` is 
the package `matsindf`, which I also wrote.
## Context
`matsbyname` v0.4.12 is a maintenance release to prepare for dplyr 1.0.0.
See `NEWS.md` for details. 

## Test environments (7 in total) and R CMD check results
* local macOS X install 10.15.3 (Catalina), R3.6.3
    * ERRORs: 0
    * WARNINGs: 0
    * NOTEs: 0
* TRAVIS-CI: Ubuntu 16.04.6, R3.6.2
    * ERRORs: 0
    * WARNINGs: 0
    * NOTEs: 0
* Windows (on win-builder):
    * `devtools::check_win_release()`, R version 3.6.3 (2020-02-29)
    * `devtools::check_win_devel()`, R Under development (unstable) (2020-03-11 r77925)
* rhub:
    * `devtools::check_rhub()`
        * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
        * Ubuntu Linux 16.04 LTS, R-release, GCC
        * Fedora Linux, R-devel, clang, gfortran

## Downstream dependencies
* Downstream dependencies were checked with `revdepcheck::revdep_check(num_workers = 4)`. 
    * Result: *Wow, no problems at all. :)*
    * BROKEN: 0
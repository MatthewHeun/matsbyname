## Context
`matsbyname` v0.4.13 is a bug fix release.
See `NEWS.md` for details. 

## Test environments (11 in total) and R CMD check results
* local macOS X install 10.15.4 (Catalina), R3.6.3
    * ERRORs: 0
    * WARNINGs: 0
    * NOTEs: 0
* TRAVIS-CI: Ubuntu 16.04.6, R3.6.2
* Windows (on win-builder):
    * `devtools::check_win_release()`, R version 3.6.3 (2020-02-29)
    * `devtools::check_win_devel()`, R Under development (unstable) (2020-03-11 r77925)
* rhub:
    * `devtools::check_rhub()`
        * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
        * Ubuntu Linux 16.04 LTS, R-release, GCC
        * Fedora Linux, R-devel, clang, gfortran
* GitHub actions:
    * 
    * 
    * 
    * 

## Downstream dependencies
* Downstream dependencies were checked with `revdepcheck::revdep_check(num_workers = 4)`. 
    * Result: 
        * Wow, no problems at all. :)
        * ✓ matsindf 0.3.3 ── E: 0     | W: 0     | N: 0
        * OK: 1
        * BROKEN: 0

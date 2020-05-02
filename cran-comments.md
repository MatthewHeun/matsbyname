## Context
`matsbyname` v0.4.14 adds one new feature (aggregation) and fixes several bugs.
See `NEWS.md` for details. 

## Test environments (8 in total) and R CMD check results
* local macOS X install 10.15.4 (Catalina), R4.0.0
    * ERRORs: 0
    * WARNINGs: 0
    * NOTEs: 0
* TRAVIS-CI: Ubuntu 16.04.6, R4.0.0
    * ERRORs: 0
    * WARNINGs: 0
    * NOTEs: 0
* Windows (on win-builder):
    * `devtools::check_win_release()`, R version 3.6.3 (2020-02-29)
        * ERRORs: 
        * WARNINGs: 
        * NOTEs: 
    * `devtools::check_win_devel()`, R Under development (unstable) (2020-04-30 r78335)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
* rhub:
    * `devtools::check_rhub()`
        * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
            * ERRORs: 1
            * WARNINGs: 0
            * NOTEs: 0
            * The error is: ERROR: dependencies 'dplyr', 'purrr', 'rlang', 'stringi', 'tibble' are not available for package 'matsbyname'  This appears to be a temporary configuration problem for this test rig only.
        * Ubuntu Linux 16.04 LTS, R-release, GCC
            * ERRORs: 0
            * WARNINGs: 0
            * NOTEs: 0
        * Fedora Linux, R-devel, clang, gfortran
            * ERRORs: 0
            * WARNINGs: 0
            * NOTEs: 0
* GitHub actions:
    * macOS-latest, R version 3.6.3 (2020-02-29)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0

## Downstream dependencies
* Downstream dependencies were checked with `revdepcheck::revdep_check(num_workers = 4)`. 
    * Result: 
        * ✓ matsindf 0.3.4 ── E: 0 | W: 0 | N: 0
            * OK: 1
            * BROKEN: 0
        * failures.md
            * Wow, no problems at all. :)
        * problems.md
            * Wow, no problems at all. :)

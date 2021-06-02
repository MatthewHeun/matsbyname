## Context
`matsbyname` v0.4.16 is a bugfix release.
See `NEWS.md` for details.

This is the second submission of `matsbyname` v0.4.16,
which cleans up the `tidyverse` URL
as requested by the CRAN team
(http://www.tidyverse.org --> https://www.tidyverse.org). 

## Test environments (7 in total) and R CMD check results
* local macOS X install 10.15.6 (Catalina), R4.0.3
    * ERRORs: 0
    * WARNINGs: 0
    * NOTEs: 0
* TRAVIS-CI: Ubuntu 16.04.6, R4.0.2
    * ERRORs: 0
    * WARNINGs: 0
    * NOTEs: 0
* Windows (on win-builder):
    * `devtools::check_win_release()`, R version 4.0.3 (2020-10-10)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * `devtools::check_win_devel()`, R Under development (unstable) (2020-11-21 r79454)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
* rhub:
    * `devtools::check_rhub()`
        * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
            * ERRORs: 2
            * WARNINGs: 0
            * NOTEs: 0
            * The errors appear to be caused by mal-configuration of this test rig.
              This is the *only* rig where errors occur.
                * `#> there is no package called 'utf8'`
        * Ubuntu Linux 16.04 LTS, R-release, GCC
            * ERRORs: 0
            * WARNINGs: 0
            * NOTEs: 0
        * Fedora Linux, R-devel, clang, gfortran
            * ERRORs: 0
            * WARNINGs: 0
            * NOTEs: 1
            * The note says 
                * `#> Examples with CPU (user + system) or elapsed time > 5s`
                * The example takes 5.426 seconds.
                * This is the *only* rig where this note is generated.


## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages


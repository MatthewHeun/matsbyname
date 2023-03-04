## Context

`matsbyname` v0.6.0
adds the capability to use `Matrix` objects,
thereby enabling sparse matrices.
See `NEWS.md` for details.


## Test environments (10 in total) and R CMD check results

* Local macOS X 13.2.1 (Ventura), R4.2.2
    * ERRORs: 0
    * WARNINGs: 0
    * NOTEs: 0
* GitHub Actions: 
    * macOS-latest (release)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * ubuntu-20.04 (devel)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * ubuntu-20.04 (release)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * windows-latest (release)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
* Windows (on win-builder):
    * `devtools::check_win_release()`, R version 4.2.2 (2022-10-31 ucrt)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * `devtools::check_win_devel()`, R Under development (unstable) (2023-01-14 r83615 ucrt)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
* rhub:
    * `devtools::check_rhub()`
        * Windows Server 2022, R-devel, 64 bit
            * ERRORs: 0
            * WARNINGs: 0
            * NOTEs: 0
            * Note that Rhub reports "Error in aspell(files, ...): No suitable spell-checker program found. Execution halted"
            * This test rig appears to be misconfigured.
        * Ubuntu Linux 20.04.1 LTS, R-release, GCC
            * Something appears to be wrong at rhub. I receive the "We're sorry but something went wrong" message for this build.
        * Fedora Linux, R-devel, clang, gfortran
            * Something appears to be wrong at rhub. I receive the "We're sorry but something went wrong" message for this build.
            

## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

 

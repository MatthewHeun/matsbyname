## Context

`matsbyname` v0.5.1
adds several enhancements for matrix inversion functions
and responds to deprecations in `tidyselect` and `purrr`.
See `NEWS.md` for details.


## Test environments (10 in total) and R CMD check results

* Local macOS X 13.1 (Ventura), R4.2.2
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
    * `devtools::check_win_release()`, R version 4.1.3 (2022-03-10)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * `devtools::check_win_devel()`, R version 4.2.2 (2022-10-31 ucrt)
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
            * ERRORs: 0
            * WARNINGs: 0
            * NOTEs: 0
        * Fedora Linux, R-devel, clang, gfortran
            * ERRORs: 0
            * WARNINGs: 0
            * NOTEs: 0
            

## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

 

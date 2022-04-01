## Context

`matsbyname` v0.5.0
adds several new functions for aggregations, 
adds two new vignettes, and
adapts to functions that have been moved to `RCLabels`.
See `NEWS.md` for details.


## Test environments (10 in total) and R CMD check results

* Local macOS X 12.3 (Monterey), R4.1.3
    * ERRORs: 0
    * WARNINGs: 0
    * NOTEs: 0
* GitHub Actions: 
    * windows-latest (release)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * macOS-latest (release)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * ubuntu-20.04 (release)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * ubuntu-20.04 (devel)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
* Windows (on win-builder):
    * `devtools::check_win_release()`, R version 4.1.3 (2022-03-10)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * `devtools::check_win_devel()`, R version 4.2.0 alpha (2022-03-31 r82049 ucrt)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
* rhub:
    * `devtools::check_rhub()`
        * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
            * ERRORs: 0
            * WARNINGs: 0
            * NOTEs: 1
                * checking for detritus in the temp directory ... NOTE  
                  Found the following files/directories:  
                  'lastMiKTeXException'  
                  This appears to be a problem with the cleanup process.  
                  This note occurs on only this test platform.  
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
 

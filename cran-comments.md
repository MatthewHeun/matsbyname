## Context

`matsbyname` v0.6.15
adds new function `mat_from_store_byname()`
and deprecates its predecessor `vec_from_store_byname()`.


## Test environments (14 in total) and R CMD check results

* Local macOS installation Tahoe 26.6.2, R4.6.1 (2026-06-24)
    * ERRORs: 0
    * WARNINGs: 0
    * NOTEs: 0
* GitHub Actions:
    * macOS-latest (release)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * windows-latest (release)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * ubuntu-latest (devel)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * ubuntu-latest (release)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * ubuntu-latest (oldrel-1)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
* rhub via rhub::rhub_check(branch = "release-0.6.15")
    * rhub linux (R-devel)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * rhub m1-san (R-devel)
        * ERRORs: 1
          ! Failed to build source package Hmisc.
          This is not my error but rather
          a mis-configuration of the m1-san test environment.
        * WARNINGs: 0
        * NOTEs: 0
    * rhub macos (R-devel)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * rhub macos-arm64 (R-devel)  
        * ERRORs: 1
          ERROR: compilation failed for package ‘Hmisc’
          This is not my error but rather
          a mis-configuration of the macos-arm64 test environment.
        * WARNINGs: 0
        * NOTEs: 0
    * rhub windows (R-devel)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0  
* Windows (on win-builder)
    * `devtools::check_win_release()`, R version 4.6.1 (2026-06-24 ucrt)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * `devtools::check_win_devel()`, R Under development (unstable) (2026-09-21 r90579 ucrt)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * `devtools::check_win_oldrelease()`, R version 4.5.3 (2026-03-11 ucrt)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0


## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages



## Context

`matsbyname` v0.6.13
updates `test-coverage.yaml` to v4, adds features, and fixed bugs.
Feature: new function `reallocate_byname()`
reallocates some entries in a matrix to other rows or columns
in proportion to remaining values in corresponding columns or rows.
Bugfix: fixed an edge-case bug in `fractionize_byname()` when 
the matrix to be fractionized had only one row or column.
See `NEWS.md` for additional details.


## Test environments (14 in total) and R CMD check results

* Local macOS X installation 14.6.1 (Sonoma), R4.5.0 (2025-04-11)
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
* rhub (routed to GitHub Actions)
    * rhub linux (R-devel)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * rhub m1-san (R-devel)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * rhub macos (R-devel)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * rhub macos-arm64 (R-devel)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * rhub windows (R-devel)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0  
* Windows (on win-builder):
    * `devtools::check_win_release()`, R version 4.4.2 (2024-10-31 ucrt)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * `devtools::check_win_devel()`, R Under development (unstable) (2025-01-16 r87584 ucrt)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * `devtools::check_win_oldrelease()`, R version 4.3.3 (2024-02-29 ucrt)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0


## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages


## Context

`matsbyname` v0.6.11
documents behaviour of `iszero_byname()` when a matrix has any `NA` elements, 
adds new functions `reallocate_byname()`, `select_rows_cols_byname()`,
`to_triplet()`, and `to_named_matrix()`.
There are bug fixes in 
`to_named_matrix()` and for some cases of setting rowtype or coltype.
See `NEWS.md` for additional details.


## Test environments (12 in total) and R CMD check results

* Local macOS X installation 14.6.1 (Sonoma), R4.4.2 (2024-10-31)
    * ERRORs: 0
    * WARNINGs: 0
    * NOTEs: 0
* GitHub Actions:
    * macOS-latest (release)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * ubuntu-latest (devel)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * ubuntu-latest (oldrel-1)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * windows-latest (release)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * ubuntu-latest (release)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * rhub linux (R-devel)
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




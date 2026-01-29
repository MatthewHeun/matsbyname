## Context

`matsbyname` v0.6.14
responds to changes in `dplyr::summarise()`,
where no more than one row can be returned per group; 
adds new function `rename_via_pattern_byname()`, 
enabling row and column renaming via regular expressions
for single matrices, lists, and columns of a data frame; and 
improved documentation for `complete_and_sort()`.


## Test environments (14 in total) and R CMD check results

* Local macOS installation 15.7.3 (Sequoia), R4.5.2 (2025-10-31)
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
    * `devtools::check_win_devel()`, R Under development (unstable) (2025-05-13 r88200 ucrt)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * `devtools::check_win_oldrelease()`, R version 4.4.3 (2025-02-28 ucrt)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 1
            * checking DESCRIPTION meta-information ... NOTE
            Author field differs from that derived from Authors@R
              Author:    'Matthew Heun [aut, cre] (ORCID: <https://orcid.org/0000-0002-7438-214X>)'
              Authors@R: 'Matthew Heun [aut, cre] (<https://orcid.org/0000-0002-7438-214X>)'
            `check_win_oldrelease()` is the only test environment where this note occurs.
        

## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages



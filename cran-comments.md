## Context

`matsbyname` v0.6.1
fixes one bug and adds a new `tol` argument
to control precision of equality determinations. 
See `NEWS.md` for details.


## Test environments (12 in total) and R CMD check results

* Local macOS X 13.3.1 (Ventura), R4.3.0
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
    * ubuntu-latest (release)
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
* Windows (on win-builder):
    * `devtools::check_win_release()`, R version 4.3.0 (2023-04-21 ucrt)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * `devtools::check_win_oldrelease()`, R version 4.2.3 (2023-03-15 ucrt)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * `devtools::check_win_devel()`, R Under development (unstable) (2023-04-24 r84317 ucrt)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
* rhub:
    * `devtools::check_rhub()`
        * Windows Server 2022, R-devel, 64 bit
            * ERRORs: 0
            * WARNINGs: 0
            * NOTEs: 1
                - checking for detritus in the temp directory ... NOTE
                - Found the following files/directories:
                - 'lastMiKTeXException'
                - This note appears to be a minor problem with the cleanup process, not caused by the `matsbyname` package itself.
        * Fedora Linux, R-devel, clang, gfortran
            * ERRORs: 0
            * WARNINGs: 0
            * NOTEs: 2
                - checking HTML version of manual ... NOTE
                - Skipping checking HTML validation: no command 'tidy' found
                - This NOTE appears to be an anomaly, as it occurs only on rhub.
                - Examples with CPU (user + system) or elapsed time > 5s
                -        user system elapsed
                - Matrix 1.37  0.053   5.852
                - This NOTE appears only in this test setup.
        * Ubuntu Linux 20.04.1 LTS, R-release, GCC
            * ERRORs: 0
            * WARNINGs: 0
            * NOTEs: 1
                - checking HTML version of manual ... NOTE
                - Skipping checking HTML validation: no command 'tidy' found
                - This NOTE appears to be an anomaly, as it occurs only on rhub.


## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
## Context

`matsbyname` v0.6.8
adds a statement of need to the `README.Rmd` file 
in preparation for submitting a paper to the 
Journal of Open Source Software. 
The GitHub pages approach has been modernized.
No other changes have been made.
See `NEWS.md` for details.


## Test environments (12 in total) and R CMD check results

* Local macOS X installation 13.5.1 (Ventura), R4.3.2 (2023-10-31)
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
    * `devtools::check_win_release()`, R version 4.3.2 (2023-10-31 ucrt)          ********************
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * `devtools::check_win_oldrelease()`, R version 4.2.3 (2023-03-15 ucrt)          ********************
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * `devtools::check_win_devel()`, R Under development (unstable) (2023-11-30 r85651 ucrt)          ********************
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
* rhub:
    * `devtools::check_rhub()`
        * Windows Server 2022, R-devel, 64 bit          ********************
            * ERRORs: 0
            * WARNINGs: 0
            * NOTEs: 2
                * checking for non-standard things in the check directory ... NOTE
                * Found the following files/directories:
                * ''NULL''
                    * This appears to be a problem with this test setup only.
                    * This is the only platform on which this NOTE occurs.
                * checking for detritus in the temp directory ... NOTE
                * Found the following files/directories:
                * 'lastMiKTeXException'
                    * This appears to be a mal-configuration of the test setup.
                    * This is the only platform on which this NOTE occurs.
        * Fedora Linux, R-devel, clang, gfortran          ********************
            * ERRORs: 0
            * WARNINGs: 0
            * NOTEs: 1
                * checking HTML version of manual ... NOTE
                * Skipping checking HTML validation: no command 'tidy' found
                    * This appears to be a val-configuration of the Linux test setups.
                    * Linux is the only platform on which this NOTE occurs.
        * Ubuntu Linux 20.04.1 LTS, R-release, GCC          ********************
            * ERRORs: 0
            * WARNINGs: 0
            * NOTEs: 1
                * checking HTML version of manual ... NOTE
                * Skipping checking HTML validation: no command 'tidy' found
                    * This appears to be a val-configuration of the Linux test setups.
                    * Linux is the only platform on which this NOTE occurs.


## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages


## Context
`matsbyname` v0.4.18 is a feature release containing a few new capabilities.
See `NEWS.md` for details.

This is the second attempt to submit v0.4.18.
The first attempt included a redirecting URL for the "stable" badge on the `readme.md` page.
That issue has been fixed for this resubmission.

## Test environments (10 in total) and R CMD check results
* local macOS X install 10.15.7 (Catalina), R4.1.0
    * ERRORs: 0
    * WARNINGs: 0
    * NOTEs: 0
* GitHub Actions: windows-latest (release)
    * ERRORs: 0
    * WARNINGs: 0
    * NOTEs: 0
* GitHub Actions: macOS-latest (release)
    * ERRORs: 0
    * WARNINGs: 0
    * NOTEs: 0
* GitHub Actions: ubuntu-20.04 (release)
    * ERRORs: 0
    * WARNINGs: 0
    * NOTEs: 0
* GitHub Actions: ubuntu-20.04 (devel)
    * ERRORs: 0
    * WARNINGs: 0
    * NOTEs: 0
* Windows (on win-builder):
    * `devtools::check_win_release()`, R version 4.1.0 (2021-05-18)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * `devtools::check_win_devel()`, R Under development (unstable) (2021-06-01 r80444)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
* rhub:
    * `devtools::check_rhub()`
        * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
            * ERRORs: 1
            * WARNINGs: 0
            * NOTEs: 0
            * The errors appear to be caused by mal-configuration of this test rig.
              This is the *only* rig where errors occur.
                * `#> Error: Bioconductor does not yet build and check packages for R version 4.2`
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


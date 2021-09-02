## Context

`matsbyname` v0.4.23 is a minor release that
introduces a helpful warning when `hatize_byname()` is called 
with a `keep` argument that is different 
from the structure of the vector.
There are no breaking changes.
See `NEWS.md` for details.

This is a re-submission to address a regression in the reverse dependency check.
Function `hatize_byname()` has been rewritten to ensure backward compatibility 
with the `matsindf` package.
See note below.


## Test environments (10 in total) and R CMD check results

* local macOS X install 10.15.7 (Catalina), R4.1.1
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
    * `devtools::check_win_release()`, R version 4.1.1 (2021-08-10)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * `devtools::check_win_devel()`, R Under development (unstable) (2021-08-30 r80832)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
* rhub:
    * `devtools::check_rhub()`
        * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
            * ERRORs: 0
            * WARNINGs: 0
            * NOTEs: 0
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
 
Note: I improved reverse dependency checking compared to the previous submission. 
I deleted the matsindf package from my machine, thereby
removing local fixes and forcing `revdepcheck` to download the CRAN version.
In that case,`revdepcheck` reported no problems.
I also reverted to the `master` branch of `matsindf` on my local machine
(to mimic the version on CRAN),
thereby mimicking how CRAN would do reverse dependency checking on `matsbyname`.
Again, `revdepcheck` reported no problems.

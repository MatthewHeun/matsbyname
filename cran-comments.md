## Context
`matsbyname` v0.4.11 is a maintenance release to prepare for R4.0.0.
See `NEWS.md` for details. 

## Test environments
* local macOS X install 10.14.6 (Mojave), R3.6.1
* ubuntu 14.04.5 (on Travis CI), R3.6.1
* windows (on win-builder)
    * `devtools::check_win_devel()`, R Under development (unstable) (2019-12-02 r77499)
* rhub
    * `devtools::check_rhub()`
        * Windows Server 2008 R2 SP1, R-devel 32/64 bit
        * Ubuntu Linux 16.04 LTS, R-release, GCC
        * Fedora Linux, R-devel, clang, gfortran

## R CMD check results
* local macOS X install 10.14.6 (Mojave), R3.6.1
    * ERRORs: 0
    * WARNINGs: 0
    * NOTEs: 0
* ubuntu 14.04.5 (on Travis CI), R3.6.1
    * ERRORs: 0
    * WARNINGs: 0
    * NOTEs: 0
* windows (on win-builder)
    * ERRORs: 0
    * WARNINGs: 0
    * NOTEs: 0
* rhub
    * Windows Server 2008 R2 SP1, R-devel 32/64 bit
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * Ubuntu Linux 16.04 LTS, R-release, GCC
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * Fedora Linux, R-devel, clang, gfortran
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0

## Downstream dependencies
* Downstream dependencies were checked with `revdepcheck::revdep_check(num_workers = 4)`. 
    * Result: *Wow, no problems at all. :)*
    * BROKEN: 0
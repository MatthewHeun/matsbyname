## Context
`matsbyname` is a new package that performs matrix mathematics by name.

## Test environments
* local macOS X install v10.14.2 (Mojave), R3.5.1
* ubuntu 14.04 (on Travis CI), R3.5.0
* win-builder via 
** `devtools::check_win_release()` (R3.5.2)
** `devtools::check_win_devel()` ()
** `devtools::check_win_oldrelease()` ()

## R CMD check results
There were no ERRORs or WARNINGs.
There were two NOTEs:

* One states (correctly) that `matsbyname` is a new submission to CRAN. 
* The second states (correctly) that my Author field differs from derived from Authors@R.
  I have include my ORCID for `pgkdown`.

## Downstream dependencies
There are currently no downstream dependencies for `matsbyname`.
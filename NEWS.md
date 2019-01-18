# matsbyname 0.4.9 (2019-01-17)

* Improved LICENSE file for submission to CRAN.


# matsbyname 0.4.8 (2019-01-16)

* Improved cran-comments for submission to CRAN..


# matsbyname 0.4.7 (2019-01-07)

* Cleaned up dependencies for testing.


# matsbyname 0.4.6 (2019-01-07)

* Now all external function calls are fully qualified. 


# matsbyname 0.4.5 (2019-01-07)

* New function `elementapply_byname()` applies a function to an element
  of a matrix specified by `row` and `col` arguments.
* Breaking changes: 
    * `elementproduct_byname()` changed to `hadamardproduct_byname()`
      to avoid name collision with `elementapply_byname()`.
    * `elementquotient_byname()` changed to `quotient_byname()`.
    * `elementpow_byname()` changed to `pow_byname()`.
    * `elementexp_byname()` changed to `exp_byname()`.


# matsbyname 0.4.4 (2019-01-02)

* Added tests to achieve 100% code coverage.


# matsbyname 0.4.3 (2019-01-02)

* `complete_rows_cols()` is now agnostic about the order of columns in `fillrow`
  and the order of rows in `fillcol`.


# matsbyname 0.4.2 (2019-01-02)

* `sort_rows_cols()` now allows entries in roworder and colorder 
  that are not presently names of rows or columns. 
  Extraneous names are silently ignored.


# matsbyname 0.4.1 (2019-01-01)

* Adding code coverage badge.


# matsbyname 0.4.0 (2018-12-27)

* Attempted first release to CRAN. (Failed.)


# matsbyname 0.3.8 (2018-12-21)

* `fractionize_byname()` now correctly handles non-square matrices.


# matsbyname 0.3.7 (2018-12-02)

* `hatinv_byname()` now handles `0` values in input vectors gracefully.
  By default, `0` values become `.Machine$double.xmax`.  
  To choose a different value, set new argument `inf_becomes` to a numerical value.
  To suppress default behavior, set `inf_becomes = NULL`.


# matsbyname 0.3.6 (2018-11-25)

* `iszero_byname()` now checks if values of `abs(a)` are `<= tol`.
   (Previously, `iszero_byname()` tested with `< tol`.)
   This change allows the zero matrix to pass the test when `tol = 0`, 
   as we would want.
* Reverted `equal_byname()` to use `isTRUE(all.equal())` when checking for equality.
* New function `identical_byname()` checks for exact equality using `identical`.
* Now up to 672 tests.


# matsbyname 0.3.5 (2018-11-18)

* Now using `identical()` instead of `isTRUE(all.equal())` for `equal_byname()` function.


# matsbyname 0.3.4 (2018-11-18)

* Added new function `hatinv_byname()`.
* Documented defaults for arguments to `count_*` functions.
* Now importing pipe operator from magrittr package at global level


# matsbyname 0.3.3 (2018-10-29)

* Fix version number on pkgdown website.
* Updated many details of pkgdown website for better user navigation.


# matsbyname 0.3.2 (2018-10-29)

* First release to CRAN didn't work.
* Added online documentation at github with pkgdown.


# matsbyname 0.3.1 (2018-08-25)

* Updated to new version of Roxygen which changed line breaks in some .Rd files.
* First release to CRAN.


# matsbyname 0.3.0 (2018-06-20)

* Removed parallelism features introduced in v0.2.6.
  Detailed timings revealed that the parallel code was slower than single-thread code.
  This topic may be revisited in the future. 
  But for now, it is best to remove the multicore code.
  So there are no longer any `mc.cores` arguments to `matsbyname` functions.


# matsbyname 0.2.9 (2018-05-24)

* Beginnings of S3 class `matbyname`. 
  Not sure if I want to keep it.
* Fixed an argument name error exposed by check.


# matsbyname 0.2.8 (2018-05-17)

* New functions `all_byname()` and `any_byname()` make logical tests easy.
* New function `replaceNaN_byname()` replaces `NaN` entries with a value (default is 0).


# matsbyname 0.2.7 (2018-04-15)

* Refactored most `*col*_byname` functions to call their respective `*row*_byname` functions
  with a transposed argument, thereby simplifying code.
* Fixed a bug caused by the above refactoring.
  In `select_cols_byname`, a `NULL` result terminated the executing thread.
* Added new function `replaceNaNWith0`.
* Added new functions `count_vals_byname`, `count_vals_inrows_byname`, and 
  `count_vals_incols_byname` that return the number of matrix entries
  that meet a criterion for the entire matrix, in each row, and in each column, respectively.
* Improvements to documentation.
* Now up to 646 passing tests.


# matsbyname 0.2.6 (2018-03-16)

## New multicore functionality available in most `*_byname` functions.

* New functions `set_mc_cores` and `get_mc_cores` to set and get package-wide `mc.cores` variable.
     Default is `1`, so all functions work as previously unless `mc.cores` is more than `1`.
     
* Alternatively, specify the `mc.cores` argument of any function 
     to specify the number of cores to be used for an individual calculation.
     Default is `get_mc_cores()`.
     A useful approach is to `set_mc_cores(detectCores(logical = FALSE))`.
     
## Suggested usage 

* `sum_byname(list(1,2,3,4), list(1,2,3,4), mc.cores = 4)` to send each sum to a different core.

* `set_mc_cores(4L); sum_byname(list(1,2,3,4), list(1,2,3,4), mc.cores = 4); set_mc_cores(1L)` 
     to do the same thing and set the package-wide value back to `1`.


# matsbyname 0.2.5 (2018-03-13)

## New `*apply_byname` functions enable API improvements

* These are API changes, but they shouldn't affect any existing code,
  because calls to binary functions will "just work."
  
* `naryapply_byname`: enables `...` arguments

* `naryapplylogical_byname`: enables logical functions

* Add `...` arguments for functions that deserve them.

## New functions with `...` arguments including

* `sum_byname`

* `matrixproduct_byname`

* `elementproduct_byname`

* `mean_byname`

* `geometricmean_byname`

* `equal_byname`

* `samestructure_byname`

## New `and_byname(...)` function that provides logical and "by name."

* The infrastructure is in place to 
  add other logical functions in the future: `or_byname`, `xor_byname`, and `not_byname`.


# matsbyname 0.2.4 (2018-03-08)

* Preparing for submission to CRAN. 
  Changed many variable names in the APIs to standardize on "a" and "b"
  as names for matrix or list of matrices arguments.


# matsbyname 0.2.3 (2018-03-08)

* Eliminate dependence (temporarily) on `matsindf`. 
  Doing so allows `matsbyname` to be submitted first to CRAN.
  Also, Travis builds are now successful, having eliminated the circular dependence between
  `matsbyname` and `matsindf`.


# matsbyname 0.2.2 (2018-03-02)

* New function `elementpow_byname` raises all elements of a matrix to a power.


# matsbyname 0.2.1 (2018-02-28)

* `complete_rows_cols` now accepts `fillrow` and `fillcol` arguments.
  These arguments can be used (instead of the `fill` argument) 
  to specify the values of filled rows and columns when completing a matrix.
  When conflicts arise, precedence among the `fill*` arguments is 
  `fillrow` then `fillcol` then `fill`.


# matsbyname 0.2.0 (2018-02-23)

* Name change to `matsbyname`.


# byname 0.1.9 (2018-02-14)

* Now preserving names of list items in `*apply_byname` functions.


# byname 0.1.8 (2018-02-14)

* Added `applybyname` vignette.


# byname 0.1.7 (2018-02-14)

* `unaryapply_byname` and `binaryapply_byname` now have `.FUNdots` arguments
  through which arguments to `FUN` should be passed. 
  Use of the `...` argument is no longer possible.
  `...` is reserved for future changes to allow an unlimited number of arguments
  to some functions, such as `sum_byname`.
* The implementation of the `.FUNdots` argument fixed a bug 
  where calculations were incorrect when 
  lists of matrices were stored in cells of a data frame. 
  Distribution of arguments (such as `margin = c(1, 2)`) across rows of a data frame
  was not happening properly.


# byname 0.1.6 (2018-02-08)

* New functions `cumsum_byname`, `cumprod_byname`, and `cumapply_byname`.
* Miscellaneous improvements to documentation of many functions.


# byname 0.1.5 (2018-02-01)

* New functions `elementlog_byname` and `elementexp_byname`.


# byname 0.1.4 (2018-01-31)

* New functions `unaryapply_byname` and `binaryapply_byname`.
  These functions have a `FUN` argument that allows an arbitrary function to be 
  applied `_byname` to matrices or data frames containing matrices.
   + `unaryapply_byname` is for unary functions such as `rowsums_byname`.
   + `binaryapply_byname` is for binary functions such as `sum_byname`.
* `unaryapply_byname` and `binaryapply_byname` are used by all `_byname` functions internally.
* Now conducting 412 tests across the entire package. 
  All tests are passing, indicating that the `unaryapply` and `binaryapply` functions are 
  very solid.


# byname 0.1.3 (2018-01-27)

* Fixed a vector vs. list bug that caused failure of binary `_byname` functions when one argument
  was a list and the other was a non-constant numeric vector. 
* Simplified `complete_rows_cols_byname`. It no longer takes a `names` argument.
* Various other fixes.


# byname 0.1.2 (2018-01-23)

* Added the following functions:
   + `mean_byname`: returns the arithmetic mean of corresponding entries of two matrices
   + `geometricmean_byname`: returns the geometric mean of corresponding entries of two matrices
   + `logarithmicmean_byname`: returns the logarithmic mean of corresponding entries of two matrices
* Fixed a bug whereby calling `setrownames_byname` and `setcolnames_byname` on a constant would fail.
  It now produces a 1x1 matrix with named rows or columns.
* Miscellaneous improvements to documentation of many functions.
   

# byname 0.1.1 (2018-01-21)

* Added the following functions: 
   + `rowprod_byname`: returns a column vector with row products (product of all entries in a row)
   + `colprod_byname`: returns a row vector with column products (product of all entries in a column)
   + `prodall_byname`: returns a numeric of the product of all entries in a matrix
* Miscellaneous improvements to documentation of many functions.


# byname 0.1

Initial version.
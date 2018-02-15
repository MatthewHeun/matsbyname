# News for `byname`

## byname 0.1.7 (2018-02-14)

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


## byname 0.1.6 (2018-02-08)

* New functions `cumsum_byname`, `cumprod_byname`, and `cumapply_byname`.
* Miscellaneous improvements to documentation of many functions.


## byname 0.1.5 (2018-02-01)

* New functions `elementlog_byname` and `elementexp_byname`.


## byname 0.1.4 (2018-01-31)

* New functions `unaryapply_byname` and `binaryapply_byname`.
  These functions have a `FUN` argument that allows an arbitrary function to be 
  applied `_byname` to matrices or data frames containing matrices.
   + `unaryapply_byname` is for unary functions such as `rowsums_byname`.
   + `binaryapply_byname` is for binary functinos such as `sum_byname`.
* `unaryapply_byname` and `binaryapply_byname` are used by all `_byname` functions internally.
* Now conducting 412 tests across the entire package. 
  All tests are passing, indicating that the `unaryapply` and `binaryapply` functions are 
  very solid.


## byname 0.1.3 (2018-01-27)

* Fixed a vector vs. list bug that caused failure of binary `_byname` functions when one argument
  was a list and the other was a non-constant numeric vector. 
* Simplified `complete_rows_cols_byname`. It no longer takes a `names` argument.
* Various other fixes.


## byname 0.1.2 (2018-01-23)

* Added the following functions:
   + `mean_byname`: returns the arithmetic mean of corresponding entries of two matrices
   + `geometricmean_byname`: returns the geometric mean of corresponding entries of two matrices
   + `logarithmicmean_byname`: returns the logarithmic mean of corresponding entries of two matrices
* Fixed a bug whereby calling `setrownames_byname` and `setcolnames_byname` on a constant would fail.
  It now produces a 1x1 matrix with named rows or columns.
* Miscellaneous improvements to documentation of many functions.
   

## byname 0.1.1 (2018-01-21)

* Added the following functions: 
   + `rowprod_byname`: returns a column vector with row products (product of all entries in a row)
   + `colprod_byname`: returns a row vector with column products (product of all entries in a column)
   + `prodall_byname`: returns a numeric of the product of all entries in a matrix
* Miscellaneous improvements to documentation of many functions.


## byname 0.1

Initial version.
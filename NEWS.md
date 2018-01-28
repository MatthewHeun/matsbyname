# News for `byname`

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
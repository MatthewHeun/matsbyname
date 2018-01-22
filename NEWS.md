# byname 0.1.2 (2018-01-22)

* Added the following functions:
   + `mean_byname`: Gives the arithmetic mean of corresponding entries of two matrices
   + `logarithmicmean_byname`: Gives the logarithmic mean of corresponding entries of two matrices
   + `geometricmean_byname`: Gives the geometric mean of corresponding entries of two matrices
* Miscellaneous improvements to documentation.
* Fixed a bug whereby calling `setrownames_byname` and `setcolnames_byname` on a constant would fail.
  It now produces a 1x1 matrix with named rows or columns.
   

# byname 0.1.1 (2018-01-21)

* Added the following functions: 
   + `rowprod_byname`: returns a column vector with row products (product of all entries in a row)
   + `colprod_byname`: returns a row vector with column products (product of all entries in a column)
   + `prodall_byname`: returns a numeric of the product of all entries in a matrix
* Miscellaneous improvements to documentation of many functions.


# byname 0.1

Initial version.
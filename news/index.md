# Changelog

## matsbyname 0.6.14 (2026-01-29)

CRAN release: 2026-01-30

- Responded to changes in
  [`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html),
  where no more than one row can be returned per group. There was an
  issue in the aggregation vignette that required a change from
  [`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
  to
  [`dplyr::reframe()`](https://dplyr.tidyverse.org/reference/reframe.html).
- New function
  [`rename_via_pattern_byname()`](https://matthewheun.github.io/matsbyname/reference/rename_via_pattern_byname.md)
  enables row and column renaming via regular expressions for single
  matrices, lists, and columns of a data frame.
- Improved documentation for
  [`complete_and_sort()`](https://matthewheun.github.io/matsbyname/reference/complete_and_sort.md).
- New tests for new features.
  - Now up to 2207 tests, all passing.
  - Test coverage remains at 100%.

## matsbyname 0.6.13 (2025-05-14) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15416297.svg)](https://doi.org/10.5281/zenodo.15416297)

CRAN release: 2025-05-14

- New function
  [`reallocate_byname()`](https://matthewheun.github.io/matsbyname/reference/reallocate_byname.md)  
  reallocates some entries in a matrix to other rows or columns in
  proportion to remaining values in corresponding columns or rows.
- Fixed an edge-case bug in
  [`fractionize_byname()`](https://matthewheun.github.io/matsbyname/reference/fractionize_byname.md)
  when the matrix to be fractionized had only one row or column.
- Additional tests for
  [`fractionize_byname()`](https://matthewheun.github.io/matsbyname/reference/fractionize_byname.md).
- Improved documentation for
  [`equal_byname()`](https://matthewheun.github.io/matsbyname/reference/equal_byname.md)
  by indicating the default value of the `.summarise` argument.
- New tests for new features.
  - Now up to 2185 tests, all passing.
  - Test coverage remains at 100%.

## matsbyname 0.6.12 (2025-03-06) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14984877.svg)](https://doi.org/10.5281/zenodo.14984877)

- Update test-coverage.yaml to v4 of the upload-artifact workflow.
- This version not released to CRAN.
- No new tests.
  - Now up to 2175 tests, all passing.
  - Test coverage remains at 100%.

## matsbyname 0.6.11 (2025-01-17) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14680392.svg)](https://doi.org/10.5281/zenodo.14680392)

CRAN release: 2025-01-17

- Fixed a bug in
  [`to_named_matrix()`](https://matthewheun.github.io/matsbyname/reference/to_named_triplet.md),
  which previously errored when a matrix with any `NA` values was passed
  in the `a` argument.
- Documented the behaviour of
  [`iszero_byname()`](https://matthewheun.github.io/matsbyname/reference/iszero_byname.md)
  when a matrix has any `NA` elements: the result is `NA`.
- Added function
  [`reallocate_byname()`](https://matthewheun.github.io/matsbyname/reference/reallocate_byname.md)
  to reallocate a row or column proportionally to other rows or columns.
- Change default name of value column in
  [`to_triplet()`](https://matthewheun.github.io/matsbyname/reference/to_named_triplet.md)
  from “x” to “value”.
- New function
  [`select_rows_cols_byname()`](https://matthewheun.github.io/matsbyname/reference/select_rows_cols_byname.md)
  includes `margin` argument that de-references row and column types, as
  needed.
- New functions
  [`to_triplet()`](https://matthewheun.github.io/matsbyname/reference/to_named_triplet.md)
  and
  [`to_named_matrix()`](https://matthewheun.github.io/matsbyname/reference/to_named_triplet.md)
  convert between named matrix and triplet forms (both integer triplet
  and character triplet) of matrices to assist some database operations
  (which prefer integer triplet form).
- Fixed a bug where setting rowtype or coltype attributes on a data
  frame resulted in the attribute set on each column of the data frame.
- New tests for new features.
  - Now up to 2175 tests, all passing.
  - Test coverage remains at 100%.

## matsbyname 0.6.10 (2024-02-12) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10652382.svg)](https://doi.org/10.5281/zenodo.10652382)

CRAN release: 2024-02-12

- Now skipping speed tests on CRAN. These tests are appropriate only for
  certain machines whose timings from previous runs are hard-wired into
  the tests. Put another way, these are not actually unit tests that
  verify correctness of code. They are timing tests. So no need to run
  these tests on CRAN (or in continuous integration tests, for that
  matter).
- No new tests.
  - Still at 2113 tests, all passing.
  - Test coverage remains at 100%.

## matsbyname 0.6.9 (2024-01-30) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10600663.svg)](https://doi.org/10.5281/zenodo.10600663)

CRAN release: 2024-01-31

- Now running tests in parallel.
- Added performance tests.
- Fixed a bug in
  [`replaceNaN_byname()`](https://matthewheun.github.io/matsbyname/reference/replaceNaN_byname.md)
  where a
  [`Matrix::sparseMatrix`](https://rdrr.io/pkg/Matrix/man/sparseMatrix.html)
  would error when nothing needed to be replaced.
- [`select_rows_byname()`](https://matthewheun.github.io/matsbyname/reference/select_rows_byname.md)
  and
  [`select_cols_byname()`](https://matthewheun.github.io/matsbyname/reference/select_cols_byname.md)
  now both
  1.  have [`grep()`](https://rdrr.io/r/base/grep.html) arguments
      `ignore.case`, `perl`, `fixed`, and `useBytes`, and
  2.  pass those arguments to
      [`grep()`](https://rdrr.io/r/base/grep.html), enabling more
      flexible matching of row and column names.
- New tests for fixed bugs and performance.
  - Now up to 2113 tests, all passing.
  - Test coverage remains at 100%.

## matsbyname 0.6.8 (2023-12-20) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10418797.svg)](https://doi.org/10.5281/zenodo.10418797)

CRAN release: 2023-12-21

- Added a statement of need.
- Updated the GitHub pages approach to use the gh-pages branch.
- No new tests.
  - Still at 2095 tests, all passing.
  - Test coverage remains at 100%.

## matsbyname 0.6.7 (2023-12-01) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10247250.svg)](https://doi.org/10.5281/zenodo.10247250)

CRAN release: 2023-12-01

- Added code of conduct and contributing pages to documentation.
- No new tests.
  - Still at 2095 tests, all passing.
  - Test coverage remains at 100%.

## matsbyname 0.6.6 (2023-10-18) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10022015.svg)](https://doi.org/10.5281/zenodo.10022015)

CRAN release: 2023-10-19

- Fixed a bug in
  [`matricize_byname()`](https://matthewheun.github.io/matsbyname/reference/matricize_byname.md).
  If the incoming matrix had `NULL` rowtype, the coltype of the outgoing
  matrix was set to [`list()`](https://rdrr.io/r/base/list.html). The
  coltype of the outgoing matrix is now set to `NULL`, as expected.
- Added new tests to verify
  [`rowtype()`](https://matthewheun.github.io/matsbyname/reference/rowtype.md)
  and
  [`coltype()`](https://matthewheun.github.io/matsbyname/reference/coltype.md)
  behavior in
  [`matrixproduct_byname()`](https://matthewheun.github.io/matsbyname/reference/matrixproduct_byname.md).
- New tests for new features.
  - Now at 2095 tests, all passing.
  - Test coverage remains at 100%.

## matsbyname 0.6.5 (2023-09-16) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.8353760.svg)](https://doi.org/10.5281/zenodo.8353760)

CRAN release: 2023-09-17

- Remove deprecated `column` argument on
  [`vec_from_store_byname()`](https://matthewheun.github.io/matsbyname/reference/vec_from_store_byname.md).
- Remove deprecated argument `matrix.class` in many places.
- Fixed tests to work with updated version of the `Matrix` package
  (1.6-2).
- Removed a few tests dealing with deprecated arguments.
  - Now at 2089 tests, all passing.
  - Test coverage is back up to 100%, due only to deprecation code.

## matsbyname 0.6.4 (2023-08-17) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.8262639.svg)](https://doi.org/10.5281/zenodo.8262639)

CRAN release: 2023-08-18

- [`matsbyname::Matrix`](https://matthewheun.github.io/matsbyname/reference/Matrix.md)
  now vectorized for lists of `matrix` objects in the `data` argument.
- Policy change:
  [`vec_from_store_byname()`](https://matthewheun.github.io/matsbyname/reference/vec_from_store_byname.md)
  now always returns a column vector.
- Deprecate the `column` argument of
  [`vec_from_store_byname()`](https://matthewheun.github.io/matsbyname/reference/vec_from_store_byname.md)
  in favor of a new `margin` argument, which is standard throughout `R`.
- Added a few new tests for
  [`vec_from_store_byname()`](https://matthewheun.github.io/matsbyname/reference/vec_from_store_byname.md)
  to verify operation when multiple rows in `a` match `v`.
- Added a new test for
  [`ncol_byname()`](https://matthewheun.github.io/matsbyname/reference/ncol_byname.md)
  to verify operation.
- New tests for new features.
  - Up to 2092 tests, all passing.
  - Test coverage is at 99.41%, due only to deprecation code.

## matsbyname 0.6.3 (2023-05-22) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7963562.svg)](https://doi.org/10.5281/zenodo.7963562)

CRAN release: 2023-05-22

- Deprecated `matrix.class` argument. It will be removed soon.
- Renamed `matrix.class` argument to `matrix_class`.
- New tests for `matrix.class` –\> `matrix_class` transition.
  - Still at 2085 tests, all passing.
  - Test coverage remains at 100%.

## matsbyname 0.6.2 (2023-05-04) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7896773.svg)](https://doi.org/10.5281/zenodo.7896773)

- Move to latest version of GitHub test coverage workflow.
- This version not released to CRAN.
- No new tests.
  - Still at 2082 tests, all passing.
  - Test coverage remains at 100%.

## matsbyname 0.6.1 (2023-04-25) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7865440.svg)](https://doi.org/10.5281/zenodo.7865440)

CRAN release: 2023-04-25

- [`equal_byname()`](https://matthewheun.github.io/matsbyname/reference/equal_byname.md)
  gains `tol` argument to control the precision with which equality is
  decided.
- Fixed a bug where
  [`equal_byname()`](https://matthewheun.github.io/matsbyname/reference/equal_byname.md)
  wasn’t using `equal_matrix_or_Matrix()`. As a result, it was difficult
  to tell whether a `matrix` and a `Matrix` were equal.
- New tests for new features.
  - Now at 2082 tests, all passing.
  - Test coverage remains at 100%.

## matsbyname 0.6.0 (2023-03-04) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7699156.svg)](https://doi.org/10.5281/zenodo.7699156)

CRAN release: 2023-03-04

- Fixed a bug in
  [`aggregate_pieces_byname()`](https://matthewheun.github.io/matsbyname/reference/aggregate_pieces_byname.md)
  where failure occurred if there were repeated row (or column) names
  not included in the aggregation_map.
- All functions now work with `Matrix` objects, thereby enabling use of
  sparse matrices. Use
  [`matsbyname::Matrix()`](https://matthewheun.github.io/matsbyname/reference/Matrix.md)
  to create sparse matrices if desired.
- [`create_matrix_byname()`](https://matthewheun.github.io/matsbyname/reference/create_matrix_byname.md)
  gains `matrix.class` argument that tells whether the created object is
  of class `matrix` or `Matrix`. Default is “matrix”.
- New function
  [`is.Matrix()`](https://matthewheun.github.io/matsbyname/reference/is.Matrix.md)
  assists with determining whether an object is a `Matrix`. The base
  function[`is.matrix()`](https://rdrr.io/r/base/matrix.html) assists
  with determining whether an object is a `matrix`.
- First function
  ([`sum_byname()`](https://matthewheun.github.io/matsbyname/reference/sum_byname.md))
  now works with `Matrix` objects, paving the way for sparse matrix
  representation throughout `matsbyname`.
- In tests, cleaned up many warnings emanating from the change to
  `testthat` v3.
- Reacted to a change in grouping policy in `dplyr` that caused one test
  to issue a deprecation warning.
- Update to latest GitHub actions continuous integration workflow.
- Many new tests for new features.
  - Now at 2078 tests, all passing.
  - Test coverage remains at 100%.

## matsbyname 0.5.1 (2023-01-16) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7541455.svg)](https://doi.org/10.5281/zenodo.7541455)

CRAN release: 2023-01-16

- Responded to deprecation warnings from
  [`purrr::cross2()`](https://purrr.tidyverse.org/reference/cross.html).
- Responded to deprecation warnings from `tidyselect`.
- [`transpose_byname()`](https://matthewheun.github.io/matsbyname/reference/transpose_byname.md)
  no longer converts a constant into a 1x1 matrix without row or column
  names.
- New function
  [`select_rowcol_piece_byname()`](https://matthewheun.github.io/matsbyname/reference/select_rowcol_piece_byname.md)
  enables selecting rows and columns using the `RCLabels` notation and
  label pieces.
- New function
  [`svd_byname()`](https://matthewheun.github.io/matsbyname/reference/svd_byname.md)
  calculates singular value decomposition of a matrix.
- New argument `method` on
  [`invert_byname()`](https://matthewheun.github.io/matsbyname/reference/invert_byname.md)
  allows callers to select the method for inverting a matrix.
- New functions
  [`eigenvalues_byname()`](https://matthewheun.github.io/matsbyname/reference/eigenvalues_byname.md)
  and
  [`eigenvectors_byname()`](https://matthewheun.github.io/matsbyname/reference/eigenvectors_byname.md)
  calculate eigenvalues and eigenvectors of matrices or lists of
  matrices.
- Added `tol` argument added to
  [`invert_byname()`](https://matthewheun.github.io/matsbyname/reference/invert_byname.md).
  The value of the `tol` argument is passed to
  [`base::solve()`](https://rdrr.io/r/base/solve.html).
- [`invert_byname()`](https://matthewheun.github.io/matsbyname/reference/invert_byname.md)
  now reports names of zero rows and columns when attempting to invert a
  singular matrix. The new error message will be a huge help for
  debugging.
- New functions `select_zero_rows_byname()` and
  `select_zero_cols_byname()` assist with detecting problem areas for
  singular matrices.
- [`rowsums_byname()`](https://matthewheun.github.io/matsbyname/reference/rowsums_byname.md),
  [`colsums_byname()`](https://matthewheun.github.io/matsbyname/reference/colsums_byname.md),
  and
  [`sumall_byname()`](https://matthewheun.github.io/matsbyname/reference/sumall_byname.md)
  now correctly return only a single number when a single number is the
  input.
- [`aggregate_byname()`](https://matthewheun.github.io/matsbyname/reference/aggregate_byname.md)
  now returns its argument (`a_mat`) if a string `margin` resolves to NA
  (isn’t a row of column type).
- Aggregation vignette now demonstrates that
  - margins for renaming and aggregating can be determined from row and
    column types and
  - inference on row and column label notation.
- Renaming and aggregation functions now (by default) infer row and
  column notations.
- Many new tests for new features.
  - Now at 1165 tests, all passing.
  - Test coverage remains at 100%.

## matsbyname 0.5.0 (2022-04-01) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6407149.svg)](https://doi.org/10.5281/zenodo.6407149)

CRAN release: 2022-04-01

- New format for documentation pages, including a search function!
- New vignette “Using summarise in matsbyname” clarifies issues around
  ambiguities in functions that use a `...` argument.
- `aggregation-vignette` now includes details on using
  `sum_byname(.summarise = TRUE)` with
  [`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html).
- [`sum_byname()`](https://matthewheun.github.io/matsbyname/reference/sum_byname.md),
  [`matrixproduct_byname()`](https://matthewheun.github.io/matsbyname/reference/matrixproduct_byname.md),
  [`hadamardproduct_byname()`](https://matthewheun.github.io/matsbyname/reference/hadamardproduct_byname.md),
  [`mean_byname()`](https://matthewheun.github.io/matsbyname/reference/mean_byname.md),
  [`geometricmean_byname()`](https://matthewheun.github.io/matsbyname/reference/geometricmean_byname.md),
  [`equal_byname()`](https://matthewheun.github.io/matsbyname/reference/equal_byname.md),
  [`identical_byname()`](https://matthewheun.github.io/matsbyname/reference/identical_byname.md),
  [`samestructure_byname()`](https://matthewheun.github.io/matsbyname/reference/samestructure_byname.md),
  and
  [`and_byname()`](https://matthewheun.github.io/matsbyname/reference/and_byname.md)
  all gain argument `.summarise` to signal intention to operate *down* a
  column (`.summarise = TRUE`) or along a list (`.summarise = FALSE`).
  The default value is `.summarise = FALSE`, thereby maintaining
  previous behavior.
- New functions
  [`agg_table_to_agg_map()`](https://matthewheun.github.io/matsbyname/reference/aggregation_map_helpers.md)
  and
  [`agg_map_to_agg_table()`](https://matthewheun.github.io/matsbyname/reference/aggregation_map_helpers.md)
  assist with manipulating aggregation maps.
- New vignette `aggregation-vignette` demonstrates the new aggregation
  functions.
- Functions
  [`rename_to_pref_suff_byname()`](https://matthewheun.github.io/matsbyname/reference/rename_to_pref_suff_byname.md)
  and
  [`aggregate_to_pref_suff_byname()`](https://matthewheun.github.io/matsbyname/reference/aggregate_to_pref_suff_byname.md)
  now route to new functions
  [`rename_to_piece_byname()`](https://matthewheun.github.io/matsbyname/reference/rename_to_piece_byname.md)
  and
  [`aggregate_pieces_byname()`](https://matthewheun.github.io/matsbyname/reference/aggregate_pieces_byname.md),
  thereby avoiding code duplication. This change may break some code.
  These functions now return an empty string (““) when a suffix is
  requested and one is not found. Previously, these functions returned
  the entire string when a suffix was not found.
- New function
  [`aggregate_pieces_byname()`](https://matthewheun.github.io/matsbyname/reference/aggregate_pieces_byname.md)
  brings the flexibility of the `RCLabels` to `matsbyname`.
- Remove (comment for now) notation functions in `notation.R` that have
  been moved to `RCLabels`.
- New function
  [`rename_to_piece_byname()`](https://matthewheun.github.io/matsbyname/reference/rename_to_piece_byname.md)
  will assist with renaming and aggregating according to pieces of row
  and column names.
- New function
  [`vec_from_store_byname()`](https://matthewheun.github.io/matsbyname/reference/vec_from_store_byname.md)
  creates vectors from a matrix (from which row of column names are
  taken) and a vector (which acts as a store of values) based on
  matching of pieces of the labels. This new function is made possible
  by the new `RCLabels` package.
- Notation code moved to new package, `RCLabels`.
- [`RCLabels::make_or_pattern()`](https://matthewheun.github.io/RCLabels/reference/make_or_pattern.html)
  gains new `pattern_type`, “literal”, which returns the `row_col_names`
  argument unmodified.
- [`trim_rows_cols()`](https://matthewheun.github.io/matsbyname/reference/trim_rows_cols.md)
  gains a `warn_if_a_incomplete` argument. When `TRUE`, a warning is
  issued if argument `a` is missing entries on `margin` that are present
  in `mat`.
- Many new tests for new features. But some functions have been moved to
  `RCLabels`, so the total number of tests has gone down slightly.
  - Now at 1072 tests, all passing.
  - Test coverage remains at 100%.

## matsbyname 0.4.25 (2021-10-12) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5565352.svg)](https://doi.org/10.5281/zenodo.5565352)

CRAN release: 2021-10-12

- New notation functions `preposition_notation()`, `from_notation()`,
  and `of_notation()`.
- Many new tests for new features.
  - Now at 1077 tests, all passing.
  - Test coverage remains at 100%.

## matsbyname 0.4.24 (2021-10-01) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5545893.svg)](https://doi.org/10.5281/zenodo.5545893)

CRAN release: 2021-10-02

- Added a test to trigger errors when dimnames are `NULL`.
- Fixed a bug where a 0x0 matrix was not being completed by another
  matrix.
- New function
  [`trim_rows_cols()`](https://matthewheun.github.io/matsbyname/reference/trim_rows_cols.md)
  eliminates rows and/or columns in one matrix based on another.
- Many new tests for new features.
  - Now at 1057 tests, all passing.
  - Test coverage remains at 100%.

## matsbyname 0.4.23 (2021-09-01) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5392263.svg)](https://doi.org/10.5281/zenodo.5392263)

CRAN release: 2021-09-02

- [`hatize_byname()`](https://matthewheun.github.io/matsbyname/reference/hatize_byname.md)
  now allows a missing `keep` argument, eliminating a regression in
  reverse dependency with the `matsindf` package.
- This release is for CRAN.
- One test could be deleted, because the
  [`hatize_byname()`](https://matthewheun.github.io/matsbyname/reference/hatize_byname.md)
  function is now simpler.
  - Now at 1039 tests, all passing.
  - Test coverage remains at 100%.

## matsbyname 0.4.22 (2021-07-26) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5138201.svg)](https://doi.org/10.5281/zenodo.5138201)

- Now issuing a helpful warning when
  [`hatize_byname()`](https://matthewheun.github.io/matsbyname/reference/hatize_byname.md)
  is called with a `keep` argument that is different from the structure
  of the vector. This will be a safe way to encourage callers to specify
  their expectations in the function call.
- Note this version was not released to CRAN, due to frequent revisions.
- New tests for new features.
  - Now up to 1040 tests, all passing.
  - Test coverage remains at 100%.

## matsbyname 0.4.21 (2021-07-23) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5129356.svg)](https://doi.org/10.5281/zenodo.5129356)

- [`hatize_byname()`](https://matthewheun.github.io/matsbyname/reference/hatize_byname.md)
  and
  [`hatinv_byname()`](https://matthewheun.github.io/matsbyname/reference/hatinv_byname.md)
  gain a new argument `keep` that tells whether to keep row names or
  column names when a 1x1 matrix is supplied. This feature assists with
  code that may occasionally encounter 1x1 vectors as input.
- Note this version was not released to CRAN, due to frequent revisions.
- New tests for new feature.
  - Now up to 1030 tests, all passing.
  - Test coverage remains at 100%.

## matsbyname 0.4.20 (2021-07-19) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5118873.svg)](https://doi.org/10.5281/zenodo.5118873)

CRAN release: 2021-07-21

- New function `keep_pref_suff()` keeps prefixes or suffixes of
  individual strings or lists of strings, based on the `notation`
  provided.
- New tests for new functions.
  - Now up to 1025 tests, all passing.
  - Test coverage remains at 100%.

## matsbyname 0.4.19 (2021-07-17)

CRAN release: 2021-07-18

- Breaking change: New logic for situations where prefix or suffix is
  not found in `split_pref_suff()`. Previously, `NULL` was returned for
  a missing prefix or suffix. Now, an empty string (`""`) is returned.
- New tests for whether `split_pref_suff()` works in a data frame.
- New tests for new logic.
  - Now up to 1009 tests, all passing.
  - Test coverage remains at 100%.

## matsbyname 0.4.18 (2021-06-02)

CRAN release: 2021-06-03

- New function
  [`kvec_from_template_byname()`](https://matthewheun.github.io/matsbyname/reference/kvec_from_template_byname.md)
  that creates a row or column vector from a template matrix.
- New function
  [`create_colvec_byname()`](https://matthewheun.github.io/matsbyname/reference/create_colvec_byname.md)
  builds on
  [`create_matrix_byname()`](https://matthewheun.github.io/matsbyname/reference/create_matrix_byname.md).
- New function
  [`create_rowvec_byname()`](https://matthewheun.github.io/matsbyname/reference/create_rowvec_byname.md)
  builds on
  [`create_matrix_byname()`](https://matthewheun.github.io/matsbyname/reference/create_matrix_byname.md).
- New function
  [`create_matrix_byname()`](https://matthewheun.github.io/matsbyname/reference/create_matrix_byname.md)
  that behaves much like
  [`matrix()`](https://rdrr.io/r/base/matrix.html) with “byname”
  characteristics.
- New tests for new functions.
  - Now up to 1000 tests, all passing.
  - Test coverage remains at 100%.

## matsbyname 0.4.17 (2021-04-10)

- Transition to GitHub actions for continuous integration.
- No new tests.
  - Still at 906 tests, all passing.
  - Test coverage remains at 100%.

## matsbyname 0.4.16 (2020-11-25)

CRAN release: 2020-11-26

- Moved URLs to `https://` where appropriate.
- Fixed a bug in
  [`rename_to_pref_suff_byname()`](https://matthewheun.github.io/matsbyname/reference/rename_to_pref_suff_byname.md)
  where a column vector would fail with “subscript out of bounds” error.
  The fix was to wrap the return list containing “pref” and “suff” in a
  list, as the rest of the function expected, when a single row was
  present.
- Fixed a bug in
  [`colsums_byname()`](https://matthewheun.github.io/matsbyname/reference/colsums_byname.md)
  and
  [`rowsums_byname()`](https://matthewheun.github.io/matsbyname/reference/rowsums_byname.md)
  where setting the `rowname` or `colname` argument to `NULL` did not
  result in an empty row name or column name.
- Fixed a bug in
  [`hatize_byname()`](https://matthewheun.github.io/matsbyname/reference/hatize_byname.md)
  where a 1x1 vector gave error:
  `length of 'dimnames' [1] not equal to array extent`. The solution is
  to check for 1x1 vectors and act accordingly.
- Fixed a warning emitted from `stringi`.
- New tests for bug fixes.  
  Now up to 906 tests, all passing.
- Test coverage remains at 100%.

## matsbyname 0.4.15 (2020-05-29)

CRAN release: 2020-05-29

- Added additional tests for new features.
  - Now up to 900 tests, all passing.
  - Test coverage remains at 100%.
- Added `tol` argument to
  [`clean_byname()`](https://matthewheun.github.io/matsbyname/reference/clean_byname.md),
  allowing for machine precision issues to be addressed.
- Ensured that all old functions, such as
  [`rename_to_pref_suff_byname()`](https://matthewheun.github.io/matsbyname/reference/rename_to_pref_suff_byname.md),
  [`aggregate_byname()`](https://matthewheun.github.io/matsbyname/reference/aggregate_byname.md),
  and
  [`aggregate_to_pref_suff_byname()`](https://matthewheun.github.io/matsbyname/reference/aggregate_to_pref_suff_byname.md)
  respect notation when using notation to renaming rows and columns.
- New functions for manipulating names of rows and columns:
  `notation_vec()`, `arrow_notation()`, `paren_notation()`,
  `bracket_notation()`, `split_pref_suff()`, `join_pref_suff()`,
  `flip_pref_suff()`, `switch_notation()`, and
  [`switch_notation_byname()`](https://matthewheun.github.io/matsbyname/reference/switch_notation_byname.md).
- Eliminated a warning in GitHub actions about `README.md` requiring a
  nonempty `<title>` element.

## matsbyname 0.4.14 (2020-05-01)

CRAN release: 2020-05-01

- Added additional tests for bug fixes and new features.
  - Now up to 829 tests, all passing.
  - Test coverage remains at 100%.
- Enhanced
  [`prep_vector_arg()`](https://matthewheun.github.io/matsbyname/reference/prep_vector_arg.md)
  to duplicate matrices when present as the vector_arg.
- Better error messages for
  [`sort_rows_cols()`](https://matthewheun.github.io/matsbyname/reference/sort_rows_cols.md).
  Now telling which row or column names are duplicates.
- Added function `aggregate_pref_suff_byname()` that combines
  [`rename_to_pref_suff_byname()`](https://matthewheun.github.io/matsbyname/reference/rename_to_pref_suff_byname.md)
  and
  [`aggregate_byname()`](https://matthewheun.github.io/matsbyname/reference/aggregate_byname.md).
- Fixed a crashing bug that appeared when
  [`aggregate_byname()`](https://matthewheun.github.io/matsbyname/reference/aggregate_byname.md)
  collapsed all rows or columns into a single row or single column or
  both.
- Added new function
  [`aggregate_byname()`](https://matthewheun.github.io/matsbyname/reference/aggregate_byname.md)
  which aggregates rows, columns, or both, according to an
  `aggregation_map`. I wanted to add this function for a long time, and
  I finally found a reason, namely the need to aggregate by prefixes or
  suffixes in the `IEATools` package. Furthermore, the `aggregation_map`
  idea seems to be solid. Note that `aggregation_map = NULL` (the
  default) aggregates rows with same names and columns with same names.
- Added function `rename_rowcol_to_pref_suff_byname()` which renames
  rows or columns to prefixes or suffixes in row and column names.
- Fixed a bug in
  [`clean_byname()`](https://matthewheun.github.io/matsbyname/reference/clean_byname.md)
  which caused a `NULL` response when unnamed rows or columns were
  present.
- Now using new
  [`prepare_.FUNdots()`](https://matthewheun.github.io/matsbyname/reference/prepare_.FUNdots.md)
  function in all `*apply_byname()` functions.
- Refactored new code for
  [`unaryapply_byname()`](https://matthewheun.github.io/matsbyname/reference/unaryapply_byname.md)
  into function
  [`prepare_.FUNdots()`](https://matthewheun.github.io/matsbyname/reference/prepare_.FUNdots.md),
  so it can be used in other `*apply_byname()` functions.

## matsbyname 0.4.13 (2020-04-17)

CRAN release: 2020-04-18

- Added additional tests for bug fixes.
  - Now up to 766 tests, all passing.
  - Code coverage remains at 100%.
- Fixed a bug in
  [`unaryapply_byname()`](https://matthewheun.github.io/matsbyname/reference/unaryapply_byname.md),
  which was not correctly handling a rectangular two-dimensional list of
  arguments to `FUN` supplied in `.FUNdots`. A rectangular
  two-dimensional list of arguments in `.FUNdots` is now interpreted as
  follows:
  - First dimension contains named arguments to `FUN`.
  - Second dimension contains unique values of the named arguments to be
    applied along the main argument `a`.

  The length of the first dimension of `.FUNdots` is the number of
  arguments supplied to `FUN`. The length of the second dimension of
  `.FUNdots` must be equal to the length of `a`.

## matsbyname 0.4.12 (2020-03-21)

CRAN release: 2020-03-22

- Maintenance to prepare for `dplyr` 1.0.0. Several tests and examples
  in `matsbyname` needed a column of a data frame constructed with
  [`I()`](https://rdrr.io/r/base/AsIs.html).
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  now requires all groups to have same type, but that wasn’t true in
  some tests, as some entries were `I<list>` (items in groups with more
  than one member) and others were `list` (items in single-item groups).
  The solution was to modify two test to
  1.  move from `data.frame` to `tibble` when creating the data frames
      for testing and
  2.  eliminate the use of [`I()`](https://rdrr.io/r/base/AsIs.html), as
      tibble is friendly to list columns.
- Added new function
  [`matricize_byname()`](https://matthewheun.github.io/matsbyname/reference/matricize_byname.md)
  that converts a column (or row) vector into a matrix.
  [`matricize_byname()`](https://matthewheun.github.io/matsbyname/reference/matricize_byname.md)
  is the inverse of
  [`vectorize_byname()`](https://matthewheun.github.io/matsbyname/reference/vectorize_byname.md).
- Added new function `vectorize_byname` that converts a matrix into a
  column vector.
- Added section to vignette about `matsindf`. This section could be
  re-added now that `matsindf` is now on CRAN.

## matsbyname 0.4.11 (2019-12-04)

CRAN release: 2019-12-05

- Maintenance release to get ready for R4.0.0. `matrix` objects now
  inherit from both `matrix` and `array`. Thus, code should no longer
  assume that `class(A)` returns an object of length 1 when `A` is a
  `matrix`. So, I eliminated all instances of `class(A) == "matrix"` in
  `if` statements in favor of `inherits(A, "matrix")`. See
  <https://developer.r-project.org/Blog/public/2019/11/09/when-you-think-class.-think-again/index.html>
  for more details.

## matsbyname 0.4.10 (2019-02-16)

CRAN release: 2019-02-16

- Added CRAN installation instructions to README.Rmd, now that the
  package is on CRAN.
- Added CITATION file. `citation("matsbyname")` now gives useful
  information.
- Fixed a bug in `matrixproduct_byname` in which row and column types
  were not set correctly when one operand was a `matrix` and the other
  operand was `NA`.

## matsbyname 0.4.9 (2019-01-17)

CRAN release: 2019-01-25

- Improved LICENSE file for submission to CRAN.
- First version to appear on CRAN.
- Added CRAN and lifecycle badges.

## matsbyname 0.4.8 (2019-01-16)

- Improved cran-comments.md for submission to CRAN.

## matsbyname 0.4.7 (2019-01-07)

- Cleaned up dependencies for testing.

## matsbyname 0.4.6 (2019-01-07)

- Now all external function calls are fully qualified.

## matsbyname 0.4.5 (2019-01-07)

- New function
  [`elementapply_byname()`](https://matthewheun.github.io/matsbyname/reference/elementapply_byname.md)
  applies a function to an element of a matrix specified by `row` and
  `col` arguments.
- Breaking changes:
  - `elementproduct_byname()` changed to
    [`hadamardproduct_byname()`](https://matthewheun.github.io/matsbyname/reference/hadamardproduct_byname.md)
    to avoid name collision with
    [`elementapply_byname()`](https://matthewheun.github.io/matsbyname/reference/elementapply_byname.md).
  - `elementquotient_byname()` changed to
    [`quotient_byname()`](https://matthewheun.github.io/matsbyname/reference/quotient_byname.md).
  - `elementpow_byname()` changed to
    [`pow_byname()`](https://matthewheun.github.io/matsbyname/reference/pow_byname.md).
  - `elementexp_byname()` changed to
    [`exp_byname()`](https://matthewheun.github.io/matsbyname/reference/exp_byname.md).

## matsbyname 0.4.4 (2019-01-02)

- Added tests to achieve 100% code coverage.

## matsbyname 0.4.3 (2019-01-02)

- [`complete_rows_cols()`](https://matthewheun.github.io/matsbyname/reference/complete_rows_cols.md)
  is now agnostic about the order of columns in `fillrow` and the order
  of rows in `fillcol`.

## matsbyname 0.4.2 (2019-01-02)

- [`sort_rows_cols()`](https://matthewheun.github.io/matsbyname/reference/sort_rows_cols.md)
  now allows entries in roworder and colorder that are not presently
  names of rows or columns. Extraneous names are silently ignored.

## matsbyname 0.4.1 (2019-01-01)

- Adding code coverage badge.

## matsbyname 0.4.0 (2018-12-27)

- Attempted first release to CRAN. (Failed.)

## matsbyname 0.3.8 (2018-12-21)

- [`fractionize_byname()`](https://matthewheun.github.io/matsbyname/reference/fractionize_byname.md)
  now correctly handles non-square matrices.

## matsbyname 0.3.7 (2018-12-02)

- [`hatinv_byname()`](https://matthewheun.github.io/matsbyname/reference/hatinv_byname.md)
  now handles `0` values in input vectors gracefully. By default, `0`
  values become `.Machine$double.xmax`.  
  To choose a different value, set new argument `inf_becomes` to a
  numerical value. To suppress default behavior, set
  `inf_becomes = NULL`.

## matsbyname 0.3.6 (2018-11-25)

- [`iszero_byname()`](https://matthewheun.github.io/matsbyname/reference/iszero_byname.md)
  now checks if values of `abs(a)` are `<= tol`. (Previously,
  [`iszero_byname()`](https://matthewheun.github.io/matsbyname/reference/iszero_byname.md)
  tested with `< tol`.) This change allows the zero matrix to pass the
  test when `tol = 0`, as we would want.
- Reverted
  [`equal_byname()`](https://matthewheun.github.io/matsbyname/reference/equal_byname.md)
  to use `isTRUE(all.equal())` when checking for equality.
- New function
  [`identical_byname()`](https://matthewheun.github.io/matsbyname/reference/identical_byname.md)
  checks for exact equality using `identical`.
- Now up to 672 tests.

## matsbyname 0.3.5 (2018-11-18)

- Now using [`identical()`](https://rdrr.io/r/base/identical.html)
  instead of `isTRUE(all.equal())` for
  [`equal_byname()`](https://matthewheun.github.io/matsbyname/reference/equal_byname.md)
  function.

## matsbyname 0.3.4 (2018-11-18)

- Added new function
  [`hatinv_byname()`](https://matthewheun.github.io/matsbyname/reference/hatinv_byname.md).
- Documented defaults for arguments to `count_*` functions.
- Now importing pipe operator from magrittr package at global level

## matsbyname 0.3.3 (2018-10-29)

- Fix version number on pkgdown website.
- Updated many details of pkgdown website for better user navigation.

## matsbyname 0.3.2 (2018-10-29)

- First release to CRAN didn’t work.
- Added online documentation at github with pkgdown.

## matsbyname 0.3.1 (2018-08-25)

- Updated to new version of Roxygen which changed line breaks in some
  .Rd files.
- First release to CRAN.

## matsbyname 0.3.0 (2018-06-20)

- Removed parallelism features introduced in v0.2.6. Detailed timings
  revealed that the parallel code was slower than single-thread code.
  This topic may be revisited in the future. But for now, it is best to
  remove the multicore code. So there are no longer any `mc.cores`
  arguments to `matsbyname` functions.

## matsbyname 0.2.9 (2018-05-24)

- Beginnings of S3 class `matbyname`. Not sure if I want to keep it.
- Fixed an argument name error exposed by check.

## matsbyname 0.2.8 (2018-05-17)

- New functions
  [`all_byname()`](https://matthewheun.github.io/matsbyname/reference/all_byname.md)
  and
  [`any_byname()`](https://matthewheun.github.io/matsbyname/reference/any_byname.md)
  make logical tests easy.
- New function
  [`replaceNaN_byname()`](https://matthewheun.github.io/matsbyname/reference/replaceNaN_byname.md)
  replaces `NaN` entries with a value (default is 0).

## matsbyname 0.2.7 (2018-04-15)

- Refactored most `*col*_byname` functions to call their respective
  `*row*_byname` functions with a transposed argument, thereby
  simplifying code.
- Fixed a bug caused by the above refactoring. In `select_cols_byname`,
  a `NULL` result terminated the executing thread.
- Added new function `replaceNaNWith0`.
- Added new functions `count_vals_byname`, `count_vals_inrows_byname`,
  and `count_vals_incols_byname` that return the number of matrix
  entries that meet a criterion for the entire matrix, in each row, and
  in each column, respectively.
- Improvements to documentation.
- Now up to 646 passing tests.

## matsbyname 0.2.6 (2018-03-16)

- New multicore functionality available in most `*_byname` functions.
  - New functions `set_mc_cores` and `get_mc_cores` to set and get
    package-wide `mc.cores` variable. Default is `1`, so all functions
    work as previously unless `mc.cores` is more than `1`.
  - Alternatively, specify the `mc.cores` argument of any function to
    specify the number of cores to be used for an individual
    calculation. Default is `get_mc_cores()`. A useful approach is to
    `set_mc_cores(detectCores(logical = FALSE))`.
- Suggested usage
  - `sum_byname(list(1,2,3,4), list(1,2,3,4), mc.cores = 4)` to send
    each sum to a different core.
  - `set_mc_cores(4L); sum_byname(list(1,2,3,4), list(1,2,3,4), mc.cores = 4); set_mc_cores(1L)`
    to do the same thing and set the package-wide value back to `1`.

## matsbyname 0.2.5 (2018-03-13)

- New `*apply_byname` functions enable API improvements
  - These are API changes, but they shouldn’t affect any existing code,
    because calls to binary functions will “just work.”
  - `naryapply_byname`: enables `...` arguments
  - `naryapplylogical_byname`: enables logical functions
  - Add `...` arguments for functions that deserve them.
- New functions with `...` arguments including
  - `sum_byname`
  - `matrixproduct_byname`
  - `elementproduct_byname`
  - `mean_byname`
  - `geometricmean_byname`
  - `equal_byname`
  - `samestructure_byname`
- New `and_byname(...)` function that provides logical and “by name.”
  - The infrastructure is in place to add other logical functions in the
    future: `or_byname`, `xor_byname`, and `not_byname`.

## matsbyname 0.2.4 (2018-03-08)

- Preparing for submission to CRAN. Changed many variable names in the
  APIs to standardize on “a” and “b” as names for matrix or list of
  matrices arguments.

## matsbyname 0.2.3 (2018-03-08)

- Eliminate dependence (temporarily) on `matsindf`. Doing so allows
  `matsbyname` to be submitted first to CRAN. Also, Travis builds are
  now successful, having eliminated the circular dependence between
  `matsbyname` and `matsindf`.

## matsbyname 0.2.2 (2018-03-02)

- New function `elementpow_byname` raises all elements of a matrix to a
  power.

## matsbyname 0.2.1 (2018-02-28)

- `complete_rows_cols` now accepts `fillrow` and `fillcol` arguments.
  These arguments can be used (instead of the `fill` argument) to
  specify the values of filled rows and columns when completing a
  matrix. When conflicts arise, precedence among the `fill*` arguments
  is `fillrow` then `fillcol` then `fill`.

## matsbyname 0.2.0 (2018-02-23)

- Name change to `matsbyname`.

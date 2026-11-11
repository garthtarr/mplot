## Submission

This is mplot 1.1.0, a resubmission requested by the CRAN team to fix the
"Lost braces" NOTE in the Rd files check (glmfence.Rd and lmfence.Rd). Those
pages now use proper `\eqn{}` markup.

Because several other changes have accumulated since 1.0.6, I have bumped the
version to 1.1.0. The main ones (see NEWS for the full list):

* `af()`, `vis()` and `bglmnet()` now parallelise with future/furrr instead of
  foreach/doParallel/doRNG. foreach, doParallel and doRNG are no longer
  dependencies; future and furrr are new Imports. RhpcBLASctl is a new
  Suggests and is only used if installed.
* Bug fixes in `bglmnet()` and a duplicate cluster-closure error in `af()` and
  `vis()`.
* A testthat test suite was added.

I also fixed two NOTEs found on win-builder while preparing this release: a
moved Travis-CI URL in README.md, and the `bglmnet()` example exceeding 10s
(it now runs with `cores = 1`).

## Test environments

* local macOS (aarch64), R 4.6.0: `R CMD check --as-cran`
* win-builder, R-devel
* R-hub (R Consortium runners): Windows R-devel and Linux R-devel

## R CMD check results

0 errors | 0 warnings | 0 notes on R-hub (Windows and Linux, R-devel).

The local macOS check shows 3 NOTEs that come from the local machine, not the
package: pandoc is not installed (README.md cannot be checked), no Java runtime
is available, and the installed HTML Tidy is too old.

## Reverse dependencies

There are no reverse dependencies on CRAN.

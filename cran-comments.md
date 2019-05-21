This restores an archived package to CRAN, correcting incorrectly handled 
pandoc dependencies, adding additional CI checks for pandoc issues, and 
correcting a NOTE about namespace and imports mismatch.

## Test environments

* local OS X install, R 3.5.2
* Ubuntu 16.04 (on travis-ci)
  * r-release
  * r-devel
* OS X 10.14 (on travis-ci), R-release
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes


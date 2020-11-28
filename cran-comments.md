This changes an import for textstat_readability(), reflecting changes in the
quanteda package structure.

## Test environments

* local OS X install, R 4.0.3
* Ubuntu 18.04 (on travis-ci)
  * r-release
  * r-devel
* OS X 10.15 (on travis-ci), r-release
* r-hub
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 notes

A false warning about a possible misspelling of `OpenFAIR` is triggered 
on the description. The capitalization and spelling is correct for this 
analysis.

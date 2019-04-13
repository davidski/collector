This is a re-submission of a new package.

Corrected since last submission:

- converted to undirected quotation marks in description text
- removed all function defaults that went to a user filespace. Functions 
  that write to disk now require explicit user decisions. Examples use 
  tempdir().


## Test environments

* local OS X install, R 3.5.2
* Ubuntu 16.04 (on travis-ci)
  * r-release
  * r-devel
* OS X 10.14 (on travis-ci), R-release
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release.

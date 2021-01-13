## Test environments
* local Win install, R 4.0.3
* linux Ubuntu 16.04.6 (on travis-ci), R 4.0.2
* osx (on travis-ci), R 4.0.3
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 
win-builder (devel and release) both gave the same notes. These notes flagged that 1) this package is a new submission,
2) a non-FOSS package license is used, and 3) there are possibly mis-spelled words in DESCRIPTION.

We are using the GNU General Public License v3.0, and there are no mis-spelled words in DESCRIPTION as the package name, "dampack", was incorrectly flagged.

## Downstream dependencies
There are no downstream dependencies yet (first-time submission)

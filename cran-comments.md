## Test environments
* local Win install, R 4.0.3
* linux Ubuntu 16.04.6 (on travis-ci), R 4.0.2
* osx (on travis-ci), R 4.0.3
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 
win-builder (devel and release) both gave the same notes. The following notes were produced upon checking:

1) this package is a new submission (this is correct)

2) a non-FOSS package license is used.
We are using the GNU General Public License v3.0, which is a FOSS package license according to https://svn.r-project.org/R/trunk/share/licenses/license.db

3) there are possibly mis-spelled words in DESCRIPTION.
There are no mis-spelled words in DESCRIPTION. The package name ("dampack") was incorrectly flagged.

## Downstream dependencies
There are no downstream dependencies yet (first-time submission)

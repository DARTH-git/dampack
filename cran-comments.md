## Test environments
* local Win install, R 4.0.3
* linux Ubuntu 16.04.6 (on travis-ci), R 4.0.2
* osx (on travis-ci), R 4.0.3
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 
win-builder (devel and release) both gave the same note. 
The following issues were raised in the note associated with checking CRAN incoming feasibility:

1) this package is a new submission (this is correct)

2) I followed StackOverflow instructions and Hadley's R package book's guide on how to put in the GPL-3 licensing information, 
and this seems to have triggered a note that reproduced the full text of the license.
The license.md file is an exact copy of the GPL-3 license linked on CRAN's website.

3) there are possibly mis-spelled words in DESCRIPTION.
There are no mis-spelled words in DESCRIPTION. The package name ("dampack") was incorrectly flagged.

Travis-CI checks and the local check all did not produce any ERRORs, WARNINGs, or NOTEs.

## Downstream dependencies
There are no downstream dependencies yet (first-time submission)

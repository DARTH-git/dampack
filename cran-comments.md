## Resubmission
This is a resubmission. In this version I have:

* Added \value to .Rd files regarding exported methods.

## Test environments
* local Win install, R 4.0.3
* linux Ubuntu 16.04.6 (on travis-ci), R 4.0.2
* osx (on travis-ci), R 4.0.3
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 
win-builder (devel and release) both gave the same note. 
The following issues were raised in the note associated with checking CRAN incoming feasibility:

1) this package is a new submission

This is a new submission in the sense that the package has not yet been accepted to CRAN, but this is not my first time submitting the package.

2) Possibly mis-spelled words in DESCRIPTION:
  Hunink (26:91)
  al (26:101)
  et (26:98)

Hunink is a name that is correctly spelled, and the other possibly mis-spelled words are spelled correctly.

Travis-CI checks and the local check all did not produce any ERRORs, WARNINGs, or NOTEs.

## Downstream dependencies
There are no downstream dependencies yet (first-time submission)

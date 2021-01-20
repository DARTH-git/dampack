## Resubmission
This is a resubmission. In this version I have:

* Added single quotes to the package name in the DESCRIPTION file.

* Removed the LICENSE file and removed the reference to this file in the DESCRIPTION file.

* Added a reference w/ doi for methods used in package.


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

This is a new submission in the sense that the package has not yet been accepted to CRAN, but this is not my first time submitting the package. I mistakenly deleted the file specifying that the package had already been submitted to CRAN and never committed the package after using devtools::release(). I'm not sure, but the inclusion of this file might have altered this particular note.

2) Possibly mis-spelled words in DESCRIPTION:
   Hunink (27:50)
   al (27:60)
   dampack (27:9)
   et (27:57)

Hunink is a name that is correctly spelled, and the other possibly mis-spelled words are spelled correctly.

Travis-CI checks and the local check all did not produce any ERRORs, WARNINGs, or NOTEs.

## Downstream dependencies
There are no downstream dependencies yet (first-time submission)

## Submission: dampack 1.1.0

<!-- DRAFT for the v1.1.0 release (#219). Before submitting: set the version to 1.1.0, add the
win-builder and mac-builder results below, and delete this comment. -->

This is a feature and bug-fix release. The main changes are:

* Strategy names are kept exactly as given, including spaces and special characters, instead of
  being converted with `make.names()`.
* `calculate_icers()` can add net monetary and net health benefit at a willingness-to-pay
  threshold, and `plot.icers()` can draw the frontier on the incremental cost-effectiveness plane.
* New `calculate_cers_ref()` and `calculate_cers_ref_psa()` compare every strategy with a
  user-selected reference strategy, with `print()` and `plot()` methods.
* The currency stored in a PSA object is used by every analysis, plot, and table.
* New `bcea_to_psa()` and `psa_to_bcea()` for converting to and from 'BCEA' objects
  ('BCEA' is in Suggests).
* Many bug fixes, listed in NEWS.md.

### Maintainer change

The maintainer changes from David Garibay <dgari039@uottawa.ca> to
Fernando Alarid-Escudero <falarid@stanford.edu>, who is an author of the package. David Garibay
(listed as David U. Garibay-Treviño) remains an author and will confirm the change by email.

## Test environments

* local macOS 27.0.1, R 4.5.3
* GitHub Actions: macOS (release), Windows (release), Ubuntu (devel, release, oldrel-1)
* GitHub Actions: Ubuntu (R release) with the minimum package versions in DESCRIPTION
  (ggplot2 3.5.0, scales 1.3.0)
* win-builder (devel and release): TO ADD
* mac-builder: TO ADD

## R CMD check results

0 errors | 0 warnings | 1 note

* The note is the maintainer change described above (CRAN incoming feasibility).

## Reverse dependencies

There are no reverse dependencies on CRAN.

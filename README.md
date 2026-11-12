# dampack
<!-- [![Build Status](https://www.travis-ci.com/DARTH-git/dampack.svg?branch=master)](https://www.travis-ci.com/DARTH-git/dampack) -->

<!-- badges: start -->
  [![R-CMD-check](https://github.com/DARTH-git/dampack/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/DARTH-git/dampack/actions/workflows/check-standard.yaml)
  <!-- badges: end -->
  
The Decision Analytic Modeling Package (dampack) is a suite of functions for analyzing and visualizing the health economic outputs of mathematical models. 

dampack is developed and maintained by the [Decision Analysis in R for Technologies in Health (DARTH)](https://darthworkgroup.com) workgroup; see [The DARTH team](#the-darth-team) below.

This package was developed with funding from the National Institute of Allergy and Infectious Diseases of the National Institutes of Health under award no. R01AI138783. The content of this package is solely the responsibility of the authors and does not necessarily represent the official views of the National Institutes of Health.

## Installation
``` r
# Install release version from CRAN
install.packages("dampack")
```

``` r
# Install development version from GitHub
devtools::install_github("DARTH-git/dampack")
```

Trying to install dampack from Github in a non-US locale on macOS may produce the following error:

``` 
Error: (converted from warning) Setting LC_CTYPE failed, using "C"
```

To solve this problem, run the following code in the terminal:
```
defaults write org.R-project.R force.LANG en_US.UTF-8 
```

## Vignettes
dampack has a series of vignettes designed to showcase the functionality of the package and explain its underlying methodology. 
The vignettes serve as a guide for proper usage, and it is highly recommended that new users read any relevant vignettes before using the package.
After installing the package, vignettes can be accessed by typing `vignette(topic, package = "dampack")`, where topic is a character string corresponding to the name of the vignette.
There is some overlap between the topics and functions covered in the six vignettes, and they should ideally be read in the following order:
1. basic_cea
2. psa_analysis
3. voi
4. psa_generation
5. dsa_generation

## The DARTH team

dampack is developed by members of the [DARTH workgroup](https://darthworkgroup.com), a group of researchers developing open-source tools for decision analysis in health.

**Package authors:** Fernando Alarid-Escudero, Greg Knowlton, Caleb W. Easterly, Matthew B. Kaufmann, and Eva Enns, with contributions from David Garibay (CRAN maintainer) and Mark Clements.

**DARTH workgroup members:**

- Fernando Alarid-Escudero ([@feralaes](https://github.com/feralaes))
- Eva Enns ([@evaenns](https://github.com/evaenns))
- David U. Garibay-Treviño ([@du-gartre](https://github.com/du-gartre))
- Hawre Jalal ([@hjalal](https://github.com/hjalal))
- Matthew B. Kaufmann ([@mbkauf](https://github.com/mbkauf))
- Gregory Knowlton ([@gknowlt](https://github.com/gknowlt))
- Eline Krijkamp ([@krijkamp](https://github.com/krijkamp))
- Alexandra Moskalewicz ([@amoskalewicz](https://github.com/amoskalewicz))
- Petros Pechlivanoglou ([@ppehli](https://github.com/ppehli))
- Jorge Roa ([@jurjoroa](https://github.com/jurjoroa))
- Mathyn Vervaart ([@matverv](https://github.com/matverv))
- Ervin Wu ([@ervinro](https://github.com/ervinro))
- Yansi Wu ([@yansi5](https://github.com/yansi5))

**Other code contributors:** Szu-Yu Zoe Kao ([@syzoekao](https://github.com/syzoekao)) and Michael Fairley ([@mfairley](https://github.com/mfairley)).

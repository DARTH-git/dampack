# dampack
[![Build Status](https://travis-ci.com/DARTH-git/dampack.svg?branch=master)](https://travis-ci.com/DARTH-git/dampack)

The Decision Analytic Modeling Package (dampack) is a suite of functions for analyzing and visualizing the health economic outputs of mathematical models. 

Created and maintained by Fernando Alarid-Escudero ([@feralaes](https://github.com/feralaes)), Greg Knowlton ([@gknowlt](https://github.com/gknowlt)), and Eva Enns ([@evaenns](https://github.com/evaenns)).

This package was developed with funding from the National Institutes of Allergy and Infectious Diseases of the National Institutes of Health under award no. R01AI138783. The content of this package is solely the responsibility of the authors and does not necessarily represent the official views of the National Institutes of Health.

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

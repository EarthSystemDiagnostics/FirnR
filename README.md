# FirnR: Simulate and analyse firn and ice cores

## Overview

**FirnR** is an R package that provides tools for the simulation of polar firn
and ice cores with a focus on the isotopic composition. Methods currently
implemented include the depth-dependent simulation of firn temperature, density,
and diffusion length for stable water isotopologues.

## Installation

**FirnR** is available on request from Thomas Münch at the Alfred Wegener
Institute, <Thomas.Muench@awi.de>. It is provided as a zip file, which can be
unpacked to a directory of your choice. Then, start an R session, set the
working directory to your folder with the unpacked package source code, and
install the package using `devtools`:
```
# install.packages("devtools")
# setwd("your-firnr-source-directory")
devtools::install()
```

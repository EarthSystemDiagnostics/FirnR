# FirnR: Simulate and analyse firn and ice cores

## Overview

**FirnR** is an R package that provides tools for working with and simulating of
polar firn and ice cores, with a focus on the isotopic composition.

Methods implemented include:

- depth-dependent modelling of firn density (Herron-Langway model);
- depth-dependent modelling of the diffusion length in firn for stable
  water isotopologues, both in depth and temporal units;
- working with firn/ice-core depth scales:
  - retrieving/calulating top, bottom, midpoint depths and layer thicknesses;
  - conversion between true and water-equivalent depth scales;
- estimation of firn parameter changes over depth (densification rate,
  depth scale compression, diffusion length);
- modelling of the smoothing effect of isotope profiles by firn diffusion;
- simulation of temporal changes of firn isotope profiles from advection,
  depth scale compression and diffusion;
- forward simulation of isotope profiles based on meteorological data.

## Installation
**FirnR** can be installed directly from GitHub:
```r
# install.packages("remotes")
remotes::install_github("EarthSystemDiagnostics/FirnR")
```

## Package data

**FirnR** includes example firn density data over the first 25 m of the cores
B41 and B42 (Freitag et al., 2013; Laepple et al., 2016), which were drilled in
close vicinity of Kohnen Station in Dronning Maud Land, Antarctica, in the field
season 2012/2013 as part of the ANT-Land-2012 campaign of the COFI project; see
`?b41.b42.density`.

## Vignettes

To highlight certain applications of **FirnR**, two package vignettes are provided:

- the vignette `Density and diffusion length data` introduces the firn density
  data that is provided with **FirnR** (see above) and showcases how to use
  package functions to obtain modelled density and diffusion length data;
- the vignette `Estimating and modelling temporal changes of firn properties and
  profiles` highlights the functions which can be used to estimate temporal
  changes, i.e. variations with depth, of firn properties, specifically density
  and diffusion length, and to model how these changes affect firn proxy records
  such as stable isotope profiles. Application of these methods is described in
  detail in Münch et al. (2017).

## Literature cited

Freitag, J., Kipfstuhl, S., and Laepple, T.: Core-scale radioscopic imaging: a
new method reveals density-calcium link in Antarctic firn, J. Glaciol., 59,
1009–1014, https://doi.org/10.3189/2013JoG13J028, 2013

Laepple, T., Hörhold, M., Münch, T., Freitag, J., Wegner, A., and Kipfstuhl, S.:
Layering of surface snow and firn at Kohnen Station, Antarctica: Noise or
seasonal signal?, J. Geophys. Res. Earth Surf., 121, 1849–1860,
https://doi.org/10.1002/2016JF003919, 2016

Münch, T., et al., Constraints on post-depositional isotope modifications
in East Antarctic firn from analysing temporal changes of isotope profiles,
The Cryosphere, 11(5), 2175-2188, https://doi.org/10.5194/tc-11-2175-2017,
2017

## Author and funding information

The R code in this package has been written and implemented by Dr. Thomas Münch,
with contributions by Dr. Thomas Laepple, at the Alfred Wegener Institute,
Helmholtz Centre for Polar and Marine Research (AWI), Germany. For further
information, code enhancements or potential bugs, please write to
<<thomas.muench@awi.de>>, or open an issue on the repository page.

This work was supported by Helmholtz funding through the Polar Regions and
Coasts in the Changing Earth System (PACES) programme of the Alfred Wegener
Institute, by the Initiative and Networking Fund of the Helmholtz Association
Grant VG-NH900, by the AWI strategy fund (project "COMB-i"), and by funding from
the European Research Council (ERC) under the European Union’s Horizon 2020
research and innovation programme (grant agreement no. 716092).

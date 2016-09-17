# Overview

FirnR is an R package to assist the modeling and interpretation of snow, firn and ice data including water isotopes

# License

still private in the ECUS group... later  GPL v3

# Installation 
install the devtools package
Clone the repository to the target directory
```
git clone https://USERNAME@bitbucket.org/ecus/FirnR.git
```
install the package. On the R command line call

```
install("PATH_TO_THE_FIRNR_DIRECTORY/FirnR")
```
now the package is installed and can be loaded with
```
library(FirnR)
```
and the help can be accessed
```
??FirnR
```
# Modification

The source code is in the PATH_TO_THE_FIRNR_DIRECTORY/FirnR/R directory
after modification, the documentation in the files also has to be adapted
(In Emacs-ESS C-c C-o C-o Generate/modify the Roxygen template)

Than compile the documentation again and reinstall the package
```
document("PATH_TO_THE_FIRNR_DIRECTORY/FirnR")
install("PATH_TO_THE_FIRNR_DIRECTORY/FirnR")
```

when satisified, commit the changes, and push them




# Usage
Please refer to the vignette FirnR.pdf for an overview
and the examples in the help file
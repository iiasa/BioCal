# BioCal

BioCal is an R package designed to calculate the biomass of trees based on specific inputs, including country, species, compartment, diameter (D), and height (H). The package employs robust scientific methods tailored for European tree species to estimate biomass accurately.

## Features

- Calculation of biomass based on the country, species, compartment, D, and H.
- Utilizes methodologies from established scientific research.
- Supports a wide range of European tree species.

## Methodologies

BioCal calculates biomass using two primary sources:

1. **Zianis et al. 2005**: Where available, species-specific equations from this study are used for biomass and stem volume calculations. This method is preferred for its detailed specificity for European tree species.

   > Zianis, D., Muukkonen, P., Mäkipää, R., & Mencuccini, M. (2005). Biomass and stem volume equations for tree species in Europe. Silva Fennica Monographs, 4, 63p. DOI: [10.14214/sf.sfm4](https://doi.org/10.14214/sf.sfm4)

2. **Forrester et al. 2017**: In cases where specific equations from Zianis et al. are not available, BioCal defaults to generalized biomass and leaf area allometric equations that incorporate stand structure, tree age, and climate data for European tree species. This generalized approach is based on the comprehensive study by Forrester et al.

   > Forrester, D.I., et al. (2017). Generalized biomass and leaf area allometric equations for European tree species incorporating stand structure, tree age and climate. Forest Ecology and Management, 396, 160-175. DOI: [10.1016/j.foreco.2017.04.011](http://dx.doi.org/10.1016/j.foreco.2017.04.011)

## Installation

Install BioCal from GitHub using:

```r
# If devtools is not installed
# install.packages("devtools")

devtools::install_github("iiasa/BioCal")

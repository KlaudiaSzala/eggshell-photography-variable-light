# eggshell-photography-variable-light

[![DOI](https://zenodo.org/badge/590905314.svg)](https://zenodo.org/badge/latestdoi/590905314)

Data and R code accompanying the paper: Presence of the cloud cover and elevation angle of the sun affect measurements of eggshell coloration and patterning obtained from calibrated digital images, by Klaudia Szala, Marcin Tobolka and Adrian Surmacki

There are five datasets in the "data" folder. Four files starting with "eggs" in the name are raw measurements used in the statistical analyses. Eggs were photographed alongside X-Rite ColorChecker chart and a scale bar. Photographs were normalised and eggshells' coloration and patterning were measured in the MICA Toolbox software (version 2.2.1) created by Troscianko, J. & Stevens, M. (2015), Methods in Ecology & Evolution.

"eggs_xrite_with_granularity" dataset contains measurements taken in different natural light conditions.
"eggs_xrite_humanConeModel" dataset is based on the same set of images, but converted to human cone catch model in D65 (400-700 nm) illumination.

"eggs_artificial_xrite" and "eggs_artificial_xrite_humanConeModel" files are measurements taken in constant artificial light.

Fifth file, "repeatability_results_XRite_basing", allows to reproduce Figure 4 from the manuscript.

R code saved in "anova_and_rpt_Xrite_basing" file allows to reproduce all statistical analyses performed in the study and to create plots contained in the manuscript.
R code "XRite_validation" contains a validation step basing on X-Rite ColorChecker chart. It allows to reproduce Tables A3-A4 and Figures A1-A2 from Appendix 1.


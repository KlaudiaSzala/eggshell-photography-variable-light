# eggshell-photography-variable-light

[![DOI](https://zenodo.org/badge/590905314.svg)](https://zenodo.org/badge/latestdoi/590905314)

Data and R code accompanying the paper: Presence of the cloud cover and elevation angle of the sun affect measurements of eggshell colouration and patterning obtained from calibrated digital images, by Klaudia Szala, Marcin Tobolka and Adrian Surmacki

There are five datasets in the "data" folder. Four files starting with "eggs" in the name are raw measurements used in the statistical analyses. Eggs were photographed with XRite grey standards and a scale bar. Photographs were normalised and eggshells' colouration and patterning was measured in the micaToolbox software (version 2.2.1) created by Troscianko, J. & Stevens, M. (2015), Methods in Ecology & Evolution.

"eggs_xrite_with_granularity" dataset contains measurements taken in different natural light conditions.
"eggs_xrite_humanConeModel" dataset are the same measurements, but converted into human cone catch model in D65 illumination.

"eggs_artificial_xrite" and "eggs_artificial_xrite_humanConeModel" files are measurements taken in constant artificial light.

Fifth file, "repeatability_results_XRite_basing", allows to reproduce Figure 4 from the manuscript.

R code saved in "anova_and_rpt_Xrite_basing" file allows to reproduce all statistical analyses performed in the study and to create plots used in the manuscript.


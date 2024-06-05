# afscISS

## Welsome

This repository is a storage location for AFSC survey age and length composition Input Sample Sizes (ISS) provided by the [surveyISS](https://benwilliams-noaa.github.io/surveyISS/) package.
The R package `afscISS` allows you to obtain AFSC survey ISS for the type of composition data you use in your stock assessment.
The `afscISS` package also allows you to obtain expanded age and length population numbers, mean length-at-age, and bootstrap bias statistics,
Please note that the expanded age and length population numbers are the same values that are provided in the GAP_PRODUCTS tables for age and length composition (in the AKFIN_SIZECOMP and AKFIN_AGECOMP tables). 
We recommend, unless you're using a special case function (stock complex, spatially-explicit, or conditional age-at-length) or desire pre-expansion combined-sex compositions, that you get the age and length population estimates that are then used for age/length composition in your stock assessment from the GAP_PRODUCTS tables hosted on AKFIN.
Please see the `Get output` and `Output description` vignettes for further descriptions of how to obtain the output from `surveyISS` for your stock and the description of the output files included in this repository.

### Installation:
Make sure you have installed R packages `devtools`.  
```
library(devtools)
devtools::install_github("afsc-assessments/afscISS")
```

### Website: [asfscISS](https://benwilliams-noaa.github.io/surveyISS/)

### NOAA README

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. 
All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. 
Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. 
Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce.
The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

### NOAA License

Software code created by U.S. Government employees is not subject to copyright in the United States (17 U.S.C. §105). 
The United States/Department of Commerce reserve all rights to seek and obtain copyright protection in countries other than the United States for
Software authored in its entirety by the Department of Commerce. 
To this end, the Department of Commerce hereby grants to Recipient a royalty-free, nonexclusive license to use, copy, and create derivative works of the Software outside of the United States.

<img src="https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" height="75" alt="NOAA Fisheries">

[U.S. Department of Commerce](https://www.commerce.gov/) | [National
Oceanographic and Atmospheric Administration](https://www.noaa.gov) |
[NOAA Fisheries](https://www.fisheries.noaa.gov/)

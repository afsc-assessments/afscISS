# script to build afscISS package data

# load packages ----
# devtools::unload('afscISS')
# devtools::install_github("afsc-assessments/afscISS", force = TRUE)
library(afscISS)

# write pkg data ----

afscISS::pkg_data(region = c('ai', 'ebs', 'ebs_slope', 'goa', 'nebs'))

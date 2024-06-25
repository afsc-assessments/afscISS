# script to obtain age/length input sample size from production run

# load packages ----
# devtools::unload('afscISS')
# devtools::install_github("afsc-assessments/afscISS", force = TRUE)
library(afscISS)

# write pkg data ----
## full run ----
afscISS::pkg_data(append = FALSE)

## year run ----
afscISS::pkg_data(append = TRUE)

# set inputs ----

species = 21720
region = 'goa'
comp = 'age'
sex_cat = 4
spec_case = NULL

# test get iss ----
afscISS::get_ISS(species,
                 region,
                 comp,
                 sex_cat,
                 spec_case)


# test get comps ----
afscISS::get_comp(species,
                  region,
                  comp,
                  sex_cat,
                  spec_case)

# test get bias ----
afscISS::get_bias(species,
                  region,
                  comp,
                  sex_cat,
                  spec_case)

# test get rss ----
afscISS::get_RSS(species,
                 region,
                 comp,
                 sex_cat,
                 spec_case)

# test get resampled comps ----
afscISS::get_res_comp(species,
                      region,
                      comp,
                      sex_cat,
                      spec_case)

# notes:
# caal doesn't have sex cat 4
# resampled comps doesn't have sex cat 4 either...


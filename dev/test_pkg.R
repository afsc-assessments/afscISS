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

# test get iss ----
comp = 'caal'
sex_cat = 0
spec_case = NULL
afscISS::get_ISS(species,
                 region,
                 comp,
                 sex_cat,
                 spec_case)


# test get comps ----
comp = 'caal'
sex_cat = 0
spec_case = NULL
afscISS::get_comp(species,
                  region,
                  comp,
                  sex_cat,
                  spec_case)

# test get popn ----
comp = 'length'
sex_cat = 4
spec_case = NULL
afscISS::get_popn(species,
                  region,
                  comp,
                  sex_cat,
                  spec_case)

# test get bias ----
comp = 'caal'
sex_cat = 0
spec_case = NULL
afscISS::get_bias(species,
                  region,
                  comp,
                  sex_cat,
                  spec_case)

# test get rss ----
comp = 'age'
sex_cat = 4
spec_case = NULL
afscISS::get_RSS(species,
                 region,
                 comp,
                 sex_cat,
                 spec_case)

# test get resampled comps ----
comp = 'length'
sex_cat = 12
spec_case = NULL
afscISS::get_bs_comp(species,
                     region,
                     comp,
                     sex_cat,
                     spec_case)

# notes:
# caal doesn't have sex cat 4
# resampled comps doesn't have sex cat 4 either...


# script to obtain age/length input sample size from production run

# load packages ----
# devtools::unload('afscISS')
# devtools::install_github("afsc-assessments/afscISS", force = TRUE)
library(afscISS)

# set inputs ----

## standard iss/comps example ----
species = 21720 # pcod
region = 'goa'
comp = 'age'
sex_cat = 4

# iss
afscISS::get_ISS(species,
                 region,
                 comp,
                 sex_cat)

# composition
afscISS::get_comp(species,
                  region,
                  comp,
                  sex_cat)

# pop'n
afscISS::get_popn(species,
                  region,
                  comp,
                  sex_cat)

## sex-specific iss/comps example ----
species = 21720 # pcod
region = 'goa'
comp = 'age'
sex_cat = 12

# iss
afscISS::get_ISS(species,
                 region,
                 comp,
                 sex_cat)

# composition
afscISS::get_comp(species,
                  region,
                  comp,
                  sex_cat)

# pop'n
afscISS::get_popn(species,
                  region,
                  comp,
                  sex_cat)

## spatial sex-specific iss/comps example ----
species = 21720 # pcod
region = 'goa'
comp = 'age'
sex_cat = 12
spec_case = 'w_c_egoa'

# iss
afscISS::get_ISS(species,
                 region,
                 comp,
                 sex_cat,
                 spec_case)

# comps
afscISS::get_comp(species,
                  region,
                  comp,
                  sex_cat,
                  spec_case)


## caal iss/comps example ----
species = 21720 # pcod
region = 'goa'
comp = 'caal'
sex_cat = 0

# iss
afscISS::get_ISS(species,
                 region,
                 comp,
                 sex_cat,
                 spec_case)

# comps
afscISS::get_comp(species,
                  region,
                  comp,
                  sex_cat,
                  spec_case)



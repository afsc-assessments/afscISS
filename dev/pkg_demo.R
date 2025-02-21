# script to obtain age/length input sample size from production run

# load packages ----
# devtools::unload('afscISS')
# devtools::install_github("afsc-assessments/afscISS", force = TRUE)
library(afscISS)

# get comp data ----
## age comps ----
afscISS::get_comp(species = 10200,
                  region = 'goa',
                  comp = 'age',
                  sex_cat = 12,
                  spec_case = 'wc_egoa')
## length comps ----
afscISS::get_comp(species = 10200,
                  region = 'goa',
                  comp = 'length',
                  sex_cat = 12,
                  spec_case = 'wc_egoa')
## caal ----
afscISS::get_comp(species = 10200,
                  region = 'goa',
                  comp = 'caal',
                  sex_cat = 2,
                  spec_case = 'wc_egoa')




# get iss ----
## age comps ----
afscISS::get_ISS(species = 10200,
                 region = 'goa',
                 comp = 'age',
                 sex_cat = 12,
                 spec_case = 'wc_egoa')
## length comps ----
afscISS::get_ISS(species = 10200,
                 region = 'goa',
                 comp = 'length',
                 sex_cat = 12,
                 spec_case = 'wc_egoa')

## caal ----
afscISS::get_ISS(species = 10200,
                 region = 'goa',
                 comp = 'caal',
                 sex_cat = 2,
                 spec_case = 'wc_egoa')






# cod bin example
afscISS::get_ISS(species = 21720,
                 region = 'nebs',
                 comp = 'caal',
                 sex_cat = 0,
                 spec_case = 'bin')


afscISS::get_comp(species = 21720,
                  region = 'nebs',
                  comp = 'length',
                  sex_cat = 4,
                  spec_case = 'bin')

# nrs example
afscISS::get_ISS(species = 10262,
                 region = 'ebs',
                 comp = 'caal',
                 sex_cat = 1,
                 spec_case = NULL)





afscISS::get_comp(species = 10262,
                 region = 'ebs',
                 comp = 'length',
                 sex_cat = 12,
                 spec_case = NULL)





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



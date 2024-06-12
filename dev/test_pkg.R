# script to obtain age/length input sample size from production run

# load packages ----
devtools::unload('afscISS')
devtools::install_github("afsc-assessments/afscISS", force = TRUE)
library(afscISS)

# get iss for goa pcod ----

afscISS::get_ISS(species = 21720,
                 region = 'goa',
                 type = 'age',
                 sex_cat = 4)

afscISS::get_ISS(species = 21720,
                 region = 'goa',
                 type = 'length',
                 sex_cat = 4)

afscISS::get_ISS(species = 21720,
                 region = 'goa',
                 type = 'age',
                 sex_cat = 4,
                 spec_case = 'w_c_egoa')


afscISS::get_ISS(species = 21720,
                 region = 'ai',
                 type = 'age',
                 sex_cat = 4,
                 spec_case = 'ai_subreg')


afscISS::get_ISS(species = 21720,
                 region = 'goa',
                 type = 'caal',
                 sex_cat = 4)

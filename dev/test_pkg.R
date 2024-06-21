# script to obtain age/length input sample size from production run

# load packages ----
devtools::unload('afscISS')
devtools::install_github("afsc-assessments/afscISS", force = TRUE)
library(afscISS)

afscISS::pkg_dta(append = FALSE)


ai <- purrr::map(list.files(here::here('output', 'ai')), ~ vroom::vroom(here::here('output', 'ai', .)))
names(ai) <- list.files(here::here('output', 'ai')) %>% 
  stringr::str_replace(., '.csv', "")

data <- ai$prod_resampled_length_bsre

grp = c('year', 'species_code', 'length', 'sex')
column = 'abund'

data[[column]]

column = rlang::enquo(column)

data %>% 
  tidytable::group_by(grp) %>% 
  tidytable::summarise(bs_med = median(data[[column]]))

data{{column}}

get_stats <- function(data, grp, column){
  data %>% 
    tidytable::summarise(bs_med = median({{column}}),
                         bs_mu = mean({{column}}),
                         q2_5th = quantile({{column}}, 0.025),
                         q25th = quantile({{column}}, 0.25),
                         q75th = quantile({{column}}, 0.75),
                         q97_5th = quantile({{column}}, 0.975),
                         .by = grp)
}

get_stats(data = ai$prod_resampled_length_bsre,
          grp = c(year, species_code, length, sex),
          # grp = c('year', 'species_code', 'length', 'sex'),
          column = abund) %>% 
  print(n = 100)

exists(ai[[prod_resampled_length_bsre]])

length(ai)
length(ai_test)
length(ai$prod_resampled_length_bsre)
length(ai_test$prod_resampled_length_bsre)

ai_test <- within(ai, rm(prod_resampled_length_bsre))

ai %>% 
  tidytable::bind_rows(ai_test)


stringr::str_match(ai, pattern = 'resampled')

ai$prod_resampled_length_bsre %>% 
  tidytable::summarise(bs_med = median(abund),
                       .by = c(year, species_code, length, sex))







data %>% 
  tidytable::summarise(bs_med := median({{column}}),
                       .by = grp)


  tidytable::summarise(bs_med = median(data[[column]]),
                       .by = grp)
                       q2_5th = quantile(data[[column]], 0.025),
                       q5th = quantile(data[[column]], 0.05),
                       q25th = quantile(data[[column]], 0.25),
                       q75th = quantile(data[[column]], 0.75),
                       q95th = quantile(data[[column]], 0.95),
                       q97_5th = quantile(data[[column]], 0.975),
                       .by = grp)

data %>% 
  dplyr::group_by({{grp}}) %>% 
  dplyr::summarise(bs_med = median(data[[column]]))


data %>% 
  tidytable::summarise(bs_mu = mean('abund'),
                       bs_med = median(abund),
                       q2_5th = quantile(abund, 0.025),
                       q25th = quantile(abund, 0.25),
                       q75th = quantile(abund, 0.75),
                       q97_5th = quantile(abund, 0.975),
                       .by = grp)








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


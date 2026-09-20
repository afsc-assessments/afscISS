# script to build afscISS package data

# load packages ----
# pkgload::unload('afscISS')
# pak::pak("afsc-assessments/afscISS")
library(afscISS)
library(tidyverse)
# compare previous run to current run ----

# current run
new_goa <- afscISS:::get_surveyISS('goa')
new_ai <- afscISS:::get_surveyISS('ai')
new_nebs <- afscISS:::get_surveyISS('nebs')

# previous run
orig_goa_age <- afscISS::get_ISS(species = c(10110, 10130, 10180, 20510, 21720, 21740, 30060, 30420, 30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200),
                             region = 'goa',
                             comp = 'age',
                             sex_cat = 4)
orig_ai_age <- afscISS::get_ISS(species = c(10110, 10112, 21720, 21740, 21921, 30060, 30420, 30050, 30051, 30052),
                                 region = 'ai',
                                 comp = 'age',
                                 sex_cat = 4)
orig_nebs_age <- afscISS::get_ISS(species = c(10110, 10112, 10115, 10130, 10210, 10261, 10285, 21720, 21740),
                                 region = 'nebs',
                                 comp = 'age',
                                 sex_cat = 4)
orig_goa_len <- afscISS::get_ISS(species = c(10110, 10130, 10180, 20510, 21720, 21740, 30060, 30420, 30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200),
                                 region = 'goa',
                                 comp = 'length',
                                 sex_cat = 4)
orig_ai_len <- afscISS::get_ISS(species = c(10110, 10112, 21720, 21740, 21921, 30060, 30420, 30050, 30051, 30052),
                                region = 'ai',
                                comp = 'length',
                                sex_cat = 4)
orig_nebs_len <- afscISS::get_ISS(species = c(10110, 10112, 10115, 10130, 10210, 10261, 10285, 21720, 21740),
                                  region = 'nebs',
                                  comp = 'length',
                                  sex_cat = 4)

# plot for main age ISS results (combined sexes just for testing)
new_goa$prod_iss_ag %>% 
  tidytable::filter(sex == 4) %>% 
  tidytable::select(year, species_code, new_iss = iss) %>% 
  tidytable::left_join(orig_goa_age %>% 
                         tidytable::select(year, species_code, old_iss = iss)) %>% 
  tidytable::mutate(region = 'goa') %>% 
  tidytable::bind_rows(new_ai$prod_iss_ag %>% 
                         tidytable::filter(sex == 4) %>% 
                         tidytable::select(year, species_code, new_iss = iss) %>% 
                         tidytable::left_join(orig_ai_age %>% 
                                                tidytable::select(year, species_code, old_iss = iss)) %>% 
                         tidytable::mutate(region = 'ai')) %>% 
  tidytable::bind_rows(new_nebs$prod_iss_ag %>% 
                         tidytable::filter(sex == 4) %>% 
                         tidytable::select(year, species_code, new_iss = iss) %>% 
                         tidytable::left_join(orig_nebs_age %>% 
                                                tidytable::select(year, species_code, old_iss = iss)) %>% 
                         tidytable::mutate(region = 'nebs')) -> plot_test

ggplot(plot_test, aes(x = new_iss, y = old_iss, col = region)) +
  geom_abline(intercept = 0, 
              slope = 1, 
              color = "red",
              linetype = "dashed",
              linewidth = 0.5) + 
  geom_point() +
  facet_wrap(~region, ncol = 1) +
  theme_bw() +
  ylab('Old run ISS') +
  xlab('New run ISS')

# plot for main length ISS results (combined sexes just for testing)
new_goa$prod_iss_ln %>% 
  tidytable::filter(sex == 4) %>% 
  tidytable::select(year, species_code, new_iss = iss) %>% 
  tidytable::left_join(orig_goa_len %>% 
                         tidytable::select(year, species_code, old_iss = iss)) %>% 
  tidytable::mutate(region = 'goa') %>% 
  tidytable::bind_rows(new_ai$prod_iss_ln %>% 
                         tidytable::filter(sex == 4) %>% 
                         tidytable::select(year, species_code, new_iss = iss) %>% 
                         tidytable::left_join(orig_ai_len %>% 
                                                tidytable::select(year, species_code, old_iss = iss)) %>% 
                         tidytable::mutate(region = 'ai')) %>% 
  tidytable::bind_rows(new_nebs$prod_iss_ln %>% 
                         tidytable::filter(sex == 4) %>% 
                         tidytable::select(year, species_code, new_iss = iss) %>% 
                         tidytable::left_join(orig_nebs_len %>% 
                                                tidytable::select(year, species_code, old_iss = iss)) %>% 
                         tidytable::mutate(region = 'nebs')) -> plot_test

ggplot(plot_test, aes(x = new_iss, y = old_iss, col = region)) +
  geom_abline(intercept = 0, 
              slope = 1, 
              color = "red",
              linetype = "dashed",
              linewidth = 0.5) + 
  geom_point() +
  facet_wrap(~region, ncol = 1) +
  theme_bw() +
  ylab('Old run ISS') +
  xlab('New run ISS')


# write pkg data ----

afscISS:::pkg_data(region = c('ai', 'ebs', 'ebs_slope', 'goa', 'nebs'))

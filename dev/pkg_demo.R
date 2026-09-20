# script to obtain age/length input sample size from production run

# load packages ----
devtools::unload('afscISS')
pak::pak("afsc-assessments/afscISS")
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
afscISS::get_ISS(species = 21720,
                 region = 'nebs',
                 comp = 'age')



sex_spec <- afscISS::get_ISS(species = 21720,
                 region = 'nebs',
                 comp = 'age',
                 sex_cat = 12)

comb <- afscISS::get_ISS(species = 21720,
                             region = 'nebs',
                             comp = 'age')



sex_spec %>% 
  tidytable::rename('sex_specific' = iss) %>% 
  tidytable::select(year, sex_specific) %>% 
  tidytable::left_join(comb %>% 
                         tidytable::rename('combined_sex' = iss) %>% 
                         tidytable::select(year, combined_sex) ) -> dat


ggplot(dat, aes(x = combined_sex, y = sex_specific)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)



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
afscISS::get_ISS(species = 10115,
                 region = 'ebs',
                 comp = 'caal',
                 sex_cat = 12,
                 spec_case = 'turb_bin')


# nebs cod
afscISS::get_ISS(species = 21720,
                 region = 'nebs',
                 comp = 'age',
                 sex_cat = 4,
                 spec_case = 'pcod_bin') 



# ebs turbot
afscISS::get_ISS(species = 10115,
                 region = 'ebs',
                 comp = 'age',
                 sex_cat = 12,
                 spec_case = 'turb_bin')

# ebs slope turbot
afscISS::get_ISS(species = 10115,
                 region = 'ebs_slope',
                 comp = 'age',
                 sex_cat = 12,
                 spec_case = 'turb_bin')

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





specimen = vroom::vroom(here::here('data', 'ebs', 'specimen.csv')) %>% 
  tidytable::mutate(region = 'BS') %>% 
  tidytable::bind_rows(vroom::vroom(here::here('data', 'ai', 'specimen.csv')) %>% 
                         tidytable::mutate(region = 'AI')) %>% 
  tidytable::bind_rows(vroom::vroom(here::here('data', 'goa', 'specimen.csv')) %>% 
                         tidytable::mutate(region = 'GOA'))

specimen %>% 
  tidytable::filter(species_code == 21720,
                    age > 0) %>% 
  tidytable::summarise(n = .N, .by = c(year, sex, age, region)) %>% 
  tidytable::drop_na() %>% 
  tidytable::left_join(specimen %>% 
                         tidytable::filter(species_code == 21720,
                                           age > 0) %>% 
                         tidytable::summarise(n_tot = .N, .by = c(year, age, region)) %>% 
                         tidytable::drop_na()) %>% 
  tidytable::filter(sex == 2) %>% 
  tidytable::mutate(p_female = n / n_tot) -> test


test %>% 
  arrange(-n) %>% 
  filter(n > 50) %>% 
  summarise(p_fem = mean(p_female),
            sd_p = sd(p_female, na.rm = TRUE),
            .by = c(age, region)) -> test2


ggplot(test2, aes(x = age, y = p_fem, col = region)) +
  geom_point() +
  geom_abline(intercept = 0.5, slope = 0)+
  geom_errorbar(aes(ymin = p_fem - 1.96 * sd_p, 
                    ymax = p_fem + 1.96 * sd_p), 
                width = 0.1)+
  ylim(0, 1)+
  geom_smooth(linetype = 'dotted') +
  facet_wrap(~region, ncol = 1)




lfreq = vroom::vroom(here::here('data', 'ebs', 'lfreq.csv')) %>% 
  tidytable::mutate(region = 'BS') %>% 
  tidytable::bind_rows(vroom::vroom(here::here('data', 'ai', 'lfreq.csv')) %>% 
                         tidytable::mutate(region = 'AI')) %>% 
  tidytable::bind_rows(vroom::vroom(here::here('data', 'goa', 'lfreq.csv')) %>% 
                         tidytable::mutate(region = 'GOA'))

lfreq %>% 
  tidytable::filter(species_code == 21720) %>% 
  tidytable::summarise(n = sum(frequency), .by = c(year, sex, length, region)) %>% 
  tidytable::drop_na() %>% 
  tidytable::left_join(lfreq %>% 
                         tidytable::filter(species_code == 21720) %>% 
                         tidytable::summarise(n_tot = sum(frequency), .by = c(year, length, region)) %>% 
                         tidytable::drop_na()) %>% 
  tidytable::filter(sex == 2) %>% 
  tidytable::mutate(p_female = n / n_tot,
                    length = length / 10) -> test


test %>% 
  tidytable::filter(n > 50) %>% 
  tidytable::summarise(n_years = .N,
            .by = c(length, region))


test %>% 
  tidytable::filter(n_tot > 50) %>% 
  tidytable::left_join(test %>% 
                         tidytable::filter(n_tot > 50) %>% 
                         tidytable::summarise(n_years = .N,
                                              .by = c(length, region))) %>% 
  tidytable::filter(n_years > 5) %>% 
  # arrange(n_years)
  tidytable::summarise(p_fem = mean(p_female),
                       sd_p = sd(p_female, na.rm = TRUE),
                       .by = c(length, region)) -> test2

test2 %>% arrange(length)


ggplot(test2, aes(x = length, y = p_fem, col = region)) +
  geom_point() +
  geom_abline(intercept = 0.5, slope = 0)+
  geom_errorbar(aes(ymin = p_fem - 1.96 * sd_p, 
                    ymax = p_fem + 1.96 * sd_p), 
                width = 0.1)+
  ylim(0, 1)+
  geom_smooth(linetype = 'dotted') +
  facet_wrap(~region, ncol = 1)







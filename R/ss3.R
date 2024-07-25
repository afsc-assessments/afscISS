get_ss3_comp <- function(species, region, comp='age', sex_cat=12, bysex = FALSE, spec_case=NULL,
                         max_age=12, seas=1, flt=2, part=0, Ageerr=1, Lbin_lo=1, Lbin_hi=-1) {
  if(isTRUE(bysex)){cat('Ignoring the sex_cat and sex will be set as 1 & 2\n\n')}
  
  
  iss = get_ISS(species = species,
                region = region,
                comp = comp,
                sex_cat = sex_cat,
                spec_case = spec_case)
  
  if(isFALSE(bysex)){
    dat = get_comp(species = species,
                   region = region,
                   comp = comp,
                   sex_cat = sex_cat,
                   spec_case = spec_case)
  } else {
    dat = tidytable::bind_rows(get_comp(species = species,
                                        region = region,
                                        comp = comp,
                                        sex_cat = 1,
                                        spec_case = spec_case),
                               get_comp(species = species,
                                        region = region,
                                        comp = comp,
                                        sex_cat = 2,
                                        spec_case = spec_case))
  }
  
  # setup ----
  years = unique(dat$year)
  bin = 0:max_age
  nbin = length(bin)
  nyear = length(years)
  sexes = unique(dat$sex)
  nsex = length(sexes)
  
  
  # full dataset
  expand.grid(year = years, species_code = unique(dat$species_code), sex = sexes,
              age = bin) %>% 
    merge(dat, all=T) %>% 
    tidytable::left_join(tidytable::select(iss, year, iss)) %>% 
    tidytable::replace_na(list(prop = 0)) %>%
    tidytable::mutate(prop = tidytable::case_when(age>=max_age ~ sum(prop),
                                                  TRUE ~ prop),
                      .by = c(sex, year)) %>% 
    tidytable::filter(age<=max_age) -> df
  
  if(isFALSE(bysex)) {
    df %>% 
      tidytable::mutate(Seas = seas,
                        FltSrv = flt,
                        Gender = 0,
                        Part = part,
                        Ageerr = Ageerr,
                        Lbin_lo = Lbin_lo,
                        Lbin_hi = Lbin_hi) %>% 
      tidytable::pivot_wider(names_from = age, values_from = prop) %>% 
      tidytable::rename(Year=year,Nsamp=iss) %>% 
      tidytable::select(-c(species_code, sex)) %>% 
      tidytable::relocate(Year, Seas, FltSrv, Gender, Part, Ageerr, Lbin_lo, Lbin_hi, Nsamp) 
    
    
  } else {
    df %>% 
      tidytable::mutate(Seas = seas,
                        FltSrv = flt,
                        Gender = 0,
                        Part = part,
                        Ageerr = Ageerr,
                        Lbin_lo = Lbin_lo,
                        Lbin_hi = Lbin_hi) %>% 
      tidytable::filter(sex==2) %>% 
      tidytable::pivot_wider(names_from = age, values_from = prop, names_prefix = 'f') %>% 
      tidytable::left_join(df %>% 
                             tidytable::filter(sex==1) %>% 
                             tidytable::pivot_wider(names_from = age, values_from = prop, names_prefix = 'm') %>% 
                             tidytable::select(-sex)) %>% 
      tidytable::rename(Year=year, Nsamp=iss) %>% 
      tidytable::select(-c(species_code, sex)) %>% 
      tidytable::relocate(Year, Seas, FltSrv, Gender, Part, Ageerr, Lbin_lo, Lbin_hi, Nsamp) 
  }
  
  
  
}
# script to obtain age/length input sample size for production run

# load packages ----
# if you don't have the afscdata package installed, you will need to install this first:
# devtools::install_github("afsc-assessments/afscdata", force = TRUE)
# now install surveyISS:
# devtools::install_github("BenWilliams-NOAA/surveyISS", force = TRUE)
library(surveyISS)

# set iterations ----
# first, is this a full run?
full_run = FALSE
# set number of desired bootstrap iterations for full run
iters_full = 500
# set number of iterations for testing run time
iters_test = 5
# set number of iters for this run
if(isTRUE(full_run)){
  iters = iters_full
} else{
  iters = iters_test}

# get data ----
# if query = TRUE then will run data queries, if FALSE will read previously run data
# set = TRUE if first time running, or if data has changed

data <- surveyISS::query_data_t3(query = FALSE)

# start run time test ----
if(iters < iters_full){
  tictoc::tic()
}

# gulf of alaska ----
data_goa <- data$data_goa

## run for all species (and subsetting out special cases so we don't have two places with those results) ----
data_goa$cpue %>% 
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052, 30150, 30152))) -> .cpue
data_goa$lfreq %>% 
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052, 30150, 30152))) -> .lfreq
data_goa$specimen %>% 
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052, 30150, 30152))) -> .specimen
strata_data <- data_goa$strata

# age/length
surveyISS::srvy_iss(iters = iters,
                    lfreq_data = .lfreq,
                    specimen_data = .specimen,
                    cpue_data = .cpue,
                    strata_data = strata_data,
                    yrs = 1990,
                    boot_hauls = TRUE,
                    boot_lengths = TRUE,
                    boot_ages = TRUE,
                    al_var = TRUE,
                    al_var_ann = TRUE,
                    age_err = TRUE,
                    region = 'goa',
                    save_interm = TRUE,
                    save_stats = TRUE,
                    save = 'prod')

# caal
surveyISS::srvy_iss_caal(iters = iters, 
                         specimen_data = .specimen, 
                         cpue_data = .cpue, 
                         yrs = 1990,
                         boot_hauls = TRUE, 
                         boot_ages = TRUE,
                         al_var = TRUE, 
                         al_var_ann = TRUE, 
                         age_err = TRUE,
                         region = 'goa', 
                         save_interm = TRUE,
                         save_stats = TRUE,
                         save = 'prod')

## run for west of 140 ----
# note: only run for pollock
data_goa$cpue %>%
  tidytable::filter(species_code %in% c(21740)) -> .cpue_poll
data_goa$lfreq %>%
  tidytable::filter(species_code %in% c(21740)) -> .lfreq_poll
data_goa$specimen %>%
  tidytable::filter(species_code %in% c(21740)) -> .specimen_poll

surveyISS::srvy_iss_w140(iters = iters,
                         lfreq_data = .lfreq_poll,
                         specimen_data = .specimen_poll,
                         cpue_data = .cpue_poll,
                         strata_data = strata_data,
                         yrs = 1990,
                         boot_hauls = TRUE,
                         boot_lengths = TRUE,
                         boot_ages = TRUE,
                         al_var = TRUE,
                         al_var_ann = TRUE,
                         age_err = TRUE,
                         region = 'goa',
                         save_interm = TRUE,
                         save_stats = TRUE,
                         save = 'prod')

## run for goa rougheye-blackspotted stock complex ----
data_goa$cpue %>%
  tidytable::filter(species_code %in% c(30050, 30051, 30052)) -> .cpue_rebs
data_goa$lfreq %>%
  tidytable::filter(species_code %in% c(30050, 30051, 30052)) -> .lfreq_rebs
data_goa$specimen %>%
  tidytable::filter(species_code %in% c(30050, 30051, 30052)) -> .specimen_rebs

surveyISS::srvy_iss_goa_cmplx(iters = iters,
                              lfreq_data = .lfreq_rebs,
                              specimen_data = .specimen_rebs,
                              cpue_data = .cpue_rebs,
                              strata_data = strata_data,
                              yrs = 1990,
                              boot_hauls = TRUE,
                              boot_lengths = TRUE,
                              boot_ages = TRUE,
                              al_var = TRUE,
                              al_var_ann = TRUE,
                              age_err = TRUE,
                              cmplx_code = 3005012,
                              cmplx = 'rebs',
                              region = 'goa',
                              save_interm = TRUE,
                              save_stats = TRUE,
                              save = 'prod')

## run for goa dusky stock (has different historical species codes) ----
data_goa$cpue %>%
  tidytable::filter(species_code %in% c(30150, 30152)) -> .cpue_dr
data_goa$lfreq %>%
  tidytable::filter(species_code %in% c(30150, 30152)) -> .lfreq_dr
data_goa$specimen %>%
  tidytable::filter(species_code %in% c(30150, 30152)) -> .specimen_dr

surveyISS::srvy_iss_goa_cmplx(iters = iters,
                              lfreq_data = .lfreq_dr,
                              specimen_data = .specimen_dr,
                              cpue_data = .cpue_dr,
                              strata_data = strata_data,
                              yrs = 1990,
                              boot_hauls = TRUE,
                              boot_lengths = TRUE,
                              boot_ages = TRUE,
                              al_var = TRUE,
                              al_var_ann = TRUE,
                              age_err = TRUE,
                              cmplx_code = 301502,
                              cmplx = 'dr',
                              region = 'goa',
                              save_interm = TRUE,
                              save_stats = TRUE,
                              save = 'prod')

# stop run time test ----
if(iters < iters_full){
  end <- tictoc::toc(quiet = TRUE)
  runtime <- round((((as.numeric(strsplit(end$callback_msg, split = " ")[[1]][1]) / iters) * iters_full) / 60) / 60, digits = 1)
  cat("Full run of", crayon::green$bold(iters_full), "iterations will take", crayon::red$bold$underline$italic(runtime), "hours", "\u2693","\n")
} else{
  cat("All", crayon::green$bold$underline$italic('Done'), "\u2693","\n")
}

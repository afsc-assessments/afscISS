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

# gulf of alaska subregions ----
data_goa <- data$data_goa

## run for all species (and subsetting out special cases so we don't have two places with those results) ----
data_goa$cpue %>% 
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052, 30150, 30152))) -> .cpue
data_goa$lfreq %>% 
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052, 30150, 30152))) -> .lfreq
data_goa$specimen %>% 
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052, 30150, 30152))) -> .specimen
strata_data <- data_goa$strata

## run w-c-e goa ----
surveyISS::srvy_iss_goa_w_c_e(iters = iters,
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

## run wc-e goa ----
surveyISS::srvy_iss_goa_wc_e(iters = iters,
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

# Test run time ----
if(iters < iters_full){
  end <- tictoc::toc(quiet = TRUE)
  runtime <- round((((as.numeric(strsplit(end$callback_msg, split = " ")[[1]][1]) / iters) * iters_full) / 60) / 60, digits = 1)
  cat("Full run of", crayon::green$bold(iters_full), "iterations will take", crayon::red$bold$underline$italic(runtime), "hours", "\u2693","\n")
} else{
  cat("All", crayon::green$bold$underline$italic('Done'), "\u2693","\n")
}

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
iters_full = 1000
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

# n & ebs cod ----

# age/length
surveyISS::srvy_iss(iters = iters,
                    lfreq_data = data$data_nebs$lfreq %>% 
                      tidytable::filter(species_code == 21720),
                    specimen_data = data$data_nebs$specimen %>% 
                      tidytable::filter(species_code == 21720),
                    cpue_data = data$data_nebs$cpue %>% 
                      tidytable::filter(species_code == 21720),
                    strata_data = data$data_nebs$strata,
                    yrs = 1979,
                    bin = c(4.5, 9.5, 14.5, 19.5, 24.5, 29.5, 34.5, 39.5, 44.5, 49.5, 54.5, 59.5, 64.5, 69.5, 74.5, 79.5, 84.5, 89.5, 94.5, 99.5, 104.5, 109.5, 114.5, 119.5),
                    boot_hauls = TRUE,
                    boot_lengths = TRUE,
                    boot_ages = TRUE,
                    al_var = TRUE,
                    al_var_ann = TRUE,
                    age_err = TRUE,
                    region = 'nebs',
                    save_interm = TRUE,
                    save_stats = TRUE,
                    save = 'pcod_bin')

# caal
surveyISS::srvy_iss_caal(iters = iters, 
                         specimen_data = data$data_nebs$specimen %>% 
                           tidytable::filter(species_code == 21720),
                         cpue_data = data$data_nebs$cpue %>% 
                           tidytable::filter(species_code == 21720), 
                         yrs = 1979,
                         bin = c(4.5, 9.5, 14.5, 19.5, 24.5, 29.5, 34.5, 39.5, 44.5, 49.5, 54.5, 59.5, 64.5, 69.5, 74.5, 79.5, 84.5, 89.5, 94.5, 99.5, 104.5, 109.5, 114.5, 119.5),
                         boot_hauls = TRUE, 
                         boot_ages = TRUE,
                         al_var = TRUE, 
                         al_var_ann = TRUE, 
                         age_err = TRUE,
                         region = 'nebs', 
                         save_interm = TRUE,
                         save_stats = TRUE,
                         save = 'pcod_bin')

# goa cod ----

# age/length
surveyISS::srvy_iss(iters = iters,
                    lfreq_data = data$data_goa$lfreq %>% 
                      tidytable::filter(species_code == 21720),
                    specimen_data = data$data_goa$specimen %>% 
                      tidytable::filter(species_code == 21720),
                    cpue_data = data$data_goa$cpue %>% 
                      tidytable::filter(species_code == 21720),
                    strata_data = data$data_goa$strata,
                    yrs = 1990,
                    bin = seq(4.5, 105.5, 5),
                    boot_hauls = TRUE,
                    boot_lengths = TRUE,
                    boot_ages = TRUE,
                    al_var = TRUE,
                    al_var_ann = TRUE,
                    age_err = TRUE,
                    region = 'goa',
                    save_interm = TRUE,
                    save_stats = TRUE,
                    save = 'pcod_bin')

# caal
surveyISS::srvy_iss_caal(iters = iters, 
                         specimen_data = data$data_goa$specimen %>% 
                           tidytable::filter(species_code == 21720),
                         cpue_data = data$data_goa$cpue %>% 
                           tidytable::filter(species_code == 21720),
                         yrs = 1990,
                         bin = seq(4.5, 105.5, 5),
                         boot_hauls = TRUE, 
                         boot_ages = TRUE,
                         al_var = TRUE, 
                         al_var_ann = TRUE, 
                         age_err = TRUE,
                         region = 'goa', 
                         save_interm = TRUE,
                         save_stats = TRUE,
                         save = 'pcod_bin')


# ebs turbot ----

# shelf survey age/length
surveyISS::srvy_iss(iters = iters,
                    lfreq_data = data$data_ebs$lfreq %>% 
                      tidytable::filter(species_code == 10115),
                    specimen_data = data$data_ebs$specimen %>% 
                      tidytable::filter(species_code == 10115),
                    cpue_data = data$data_ebs$cpue %>% 
                      tidytable::filter(species_code == 10115),
                    strata_data = data$data_ebs$strata,
                    yrs = 1979,
                    bin = c(10, 15, 18, 21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 51, 54, 57, 60, 63, 66, 69, 72, 75, 80, 85, 90, 100),
                    boot_hauls = TRUE,
                    boot_lengths = TRUE,
                    boot_ages = TRUE,
                    al_var = TRUE,
                    al_var_ann = TRUE,
                    age_err = TRUE,
                    region = 'ebs',
                    save_interm = TRUE,
                    save_stats = TRUE,
                    save = 'turb_bin')


# slope survey age/length
surveyISS::srvy_iss(iters = iters,
                    lfreq_data = data$data_ebss$lfreq %>% 
                      tidytable::filter(species_code == 10115),
                    specimen_data = data$data_ebss$specimen %>% 
                      tidytable::filter(species_code == 10115),
                    cpue_data = data$data_ebss$cpue %>% 
                      tidytable::filter(species_code == 10115),
                    strata_data = data$data_ebss$strata,
                    yrs = 1979,
                    bin = c(10, 15, 18, 21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 51, 54, 57, 60, 63, 66, 69, 72, 75, 80, 85, 90, 100),
                    boot_hauls = TRUE,
                    boot_lengths = TRUE,
                    boot_ages = TRUE,
                    al_var = TRUE,
                    al_var_ann = TRUE,
                    age_err = TRUE,
                    region = 'ebs_slope',
                    save_interm = TRUE,
                    save_stats = TRUE,
                    save = 'turb_bin')


# goa rex sole ----

# wc-e goa age/length
surveyISS::srvy_iss_goa_wc_e(iters = iters,
                             lfreq_data = data$data_goa$lfreq %>% 
                               tidytable::filter(species_code == 10200),
                             specimen_data = data$data_goa$specimen %>% 
                               tidytable::filter(species_code == 10200),
                             cpue_data = data$data_goa$cpue %>% 
                               tidytable::filter(species_code == 10200),
                             strata_data = data$data_goa$strata,
                             yrs = 1993,
                             # bin = seq(from = 9, to = 65, by = 2), # come back to why this doesn't work when binning
                             boot_hauls = TRUE,
                             boot_lengths = TRUE,
                             boot_ages = TRUE,
                             al_var = TRUE,
                             al_var_ann = TRUE,
                             age_err = TRUE,
                             region = 'goa',
                             save_interm = TRUE,
                             save_stats = TRUE,
                             save = 'rex_bin')

# wc-e goa caal
surveyISS::srvy_iss_goa_wc_e_caal(iters = iters,
                                  specimen_data = data$data_goa$specimen %>% 
                                    tidytable::filter(species_code == 10200),
                                  cpue_data = data$data_goa$cpue %>% 
                                    tidytable::filter(species_code == 10200),
                                  strata_data = data$data_goa$strata,
                                  yrs = 1993,
                                  bin = seq(from = 9, to = 65, by = 2), 
                                  boot_hauls = TRUE,
                                  boot_ages = TRUE,
                                  al_var = TRUE,
                                  al_var_ann = TRUE,
                                  age_err = TRUE,
                                  region = 'goa',
                                  save_interm = TRUE,
                                  save_stats = TRUE,
                                  save = 'rex_bin')



# stop run time test ----
if(iters < iters_full){
  end <- tictoc::toc(quiet = TRUE)
  runtime <- round((((as.numeric(strsplit(end$callback_msg, split = " ")[[1]][1]) / iters) * iters_full) / 60) / 60, digits = 1)
  cat("Full run of", crayon::green$bold(iters_full), "iterations will take", crayon::red$bold$underline$italic(runtime), "hours", "\u2693","\n")
} else{
  cat("All", crayon::green$bold$underline$italic('Done'), "\u2693","\n")
}

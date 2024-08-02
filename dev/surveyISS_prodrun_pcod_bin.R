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

bin = c(4.5, 9.5, 14.5, 19.5, 24.5, 29.5, 34.5, 39.5, 44.5, 49.5, 54.5, 59.5, 64.5, 69.5, 74.5, 79.5, 84.5, 89.5, 94.5, 99.5, 104.5, 109.5, 114.5, 119.5)

# nbs & ebs ----
data_nebs <- data$data_nebs

data_nebs$cpue %>% 
  tidytable::filter(species_code == 21720) -> .cpue
data_nebs$lfreq %>% 
  tidytable::filter(species_code == 21720) -> .lfreq
data_nebs$specimen %>% 
  tidytable::filter(species_code == 21720) -> .specimen
strata_data <- data_nebs$strata

# age/length
surveyISS::srvy_iss(iters = iters,
                    lfreq_data = .lfreq,
                    specimen_data = .specimen,
                    cpue_data = .cpue,
                    strata_data = strata_data,
                    yrs = 1979,
                    bin = bin,
                    boot_hauls = TRUE,
                    boot_lengths = TRUE,
                    boot_ages = TRUE,
                    al_var = TRUE,
                    al_var_ann = TRUE,
                    age_err = TRUE,
                    region = 'nebs',
                    save_interm = TRUE,
                    save_stats = TRUE,
                    save = 'prod_bin')

# caal
surveyISS::srvy_iss_caal(iters = iters, 
                         specimen_data = .specimen, 
                         cpue_data = .cpue, 
                         yrs = 1979,
                         bin = bin,
                         boot_hauls = TRUE, 
                         boot_ages = TRUE,
                         al_var = TRUE, 
                         al_var_ann = TRUE, 
                         age_err = TRUE,
                         region = 'nebs', 
                         save_interm = TRUE,
                         save_stats = TRUE,
                         save = 'prod_bin')

# goa ----
data_goa <- data$data_goa

data_goa$cpue %>% 
  tidytable::filter(species_code == 21720) -> .cpue
data_goa$lfreq %>% 
  tidytable::filter(species_code == 21720) -> .lfreq
data_goa$specimen %>% 
  tidytable::filter(species_code == 21720) -> .specimen
strata_data <- data_goa$strata

# age/length
surveyISS::srvy_iss(iters = iters,
                    lfreq_data = .lfreq,
                    specimen_data = .specimen,
                    cpue_data = .cpue,
                    strata_data = strata_data,
                    yrs = 1990,
                    bin = bin,
                    boot_hauls = TRUE,
                    boot_lengths = TRUE,
                    boot_ages = TRUE,
                    al_var = TRUE,
                    al_var_ann = TRUE,
                    age_err = TRUE,
                    region = 'goa',
                    save_interm = TRUE,
                    save_stats = TRUE,
                    save = 'prod_bin')

# caal
surveyISS::srvy_iss_caal(iters = iters, 
                         specimen_data = .specimen, 
                         cpue_data = .cpue, 
                         yrs = 1990,
                         bin = bin,
                         boot_hauls = TRUE, 
                         boot_ages = TRUE,
                         al_var = TRUE, 
                         al_var_ann = TRUE, 
                         age_err = TRUE,
                         region = 'goa', 
                         save_interm = TRUE,
                         save_stats = TRUE,
                         save = 'prod_bin')

# stop run time test ----
if(iters < iters_full){
  end <- tictoc::toc(quiet = TRUE)
  runtime <- round((((as.numeric(strsplit(end$callback_msg, split = " ")[[1]][1]) / iters) * iters_full) / 60) / 60, digits = 1)
  cat("Full run of", crayon::green$bold(iters_full), "iterations will take", crayon::red$bold$underline$italic(runtime), "hours", "\u2693","\n")
} else{
  cat("All", crayon::green$bold$underline$italic('Done'), "\u2693","\n")
}

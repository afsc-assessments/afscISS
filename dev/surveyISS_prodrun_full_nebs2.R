# script to obtain age/length input sample size for production run

# load packages ----
# if you don't have the afscdata package installed, you will need to install this first:
# devtools::install_github("afsc-assessments/afscdata", force = TRUE)
# now install surveyISS:
# devtools::install_github("BenWilliams-NOAA/surveyISS", force = TRUE)
library(surveyISS)

# do you want to write package data after run?
write_data = FALSE

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

data_nebs <- data$data_nebs

# test run time ----
if(iters < iters_full){
  tictoc::tic()
}

# nbs & ebs ----
# subset to species set
data_nebs$cpue %>% 
  tidytable::filter(species_code != 21740) -> cpue
data_nebs$lfreq %>% 
  tidytable::filter(species_code != 21740) -> lfreq
data_nebs$specimen %>% 
  tidytable::filter(species_code != 21740) -> specimen

# age/length
# for comp results
surveyISS::srvy_iss(iters = iters,
                    lfreq_data = lfreq,
                    specimen_data = specimen,
                    cpue_data = cpue,
                    strata_data = data_nebs$strata,
                    yrs = 1979,
                    boot_hauls = TRUE,
                    boot_lengths = TRUE,
                    boot_ages = TRUE,
                    al_var = FALSE,
                    al_var_ann = FALSE,
                    age_err = FALSE,
                    region = 'nebs',
                    save_interm = TRUE,
                    save_stats = FALSE,
                    save = 'prod_s2')
# for stats results
surveyISS::srvy_iss(iters = iters,
                    lfreq_data = lfreq,
                    specimen_data = specimen,
                    cpue_data = cpue,
                    strata_data = data_nebs$strata,
                    yrs = 1979,
                    boot_hauls = TRUE,
                    boot_lengths = TRUE,
                    boot_ages = TRUE,
                    al_var = TRUE,
                    al_var_ann = TRUE,
                    age_err = TRUE,
                    region = 'nebs',
                    save_interm = FALSE,
                    save_stats = TRUE,
                    save = 'prod_s2')

# caal
# for comp results
surveyISS::srvy_iss_caal(iters = iters, 
                         specimen_data = specimen, 
                         cpue_data = cpue, 
                         yrs = 1979,
                         boot_hauls = TRUE, 
                         boot_ages = TRUE,
                         al_var = FALSE, 
                         al_var_ann = FALSE, 
                         age_err = FALSE,
                         region = 'nebs', 
                         save_interm = TRUE,
                         save_stats = FALSE,
                         save = 'prod_s2')
# for stats results
surveyISS::srvy_iss_caal(iters = iters, 
                         specimen_data = specimen, 
                         cpue_data = cpue, 
                         yrs = 1979,
                         boot_hauls = TRUE, 
                         boot_ages = TRUE,
                         al_var = TRUE, 
                         al_var_ann = TRUE, 
                         age_err = TRUE,
                         region = 'nebs', 
                         save_interm = FALSE,
                         save_stats = TRUE,
                         save = 'prod_s2')

# Test run time ----
if(iters < iters_full){
  end <- tictoc::toc(quiet = TRUE)
  runtime <- round((((as.numeric(strsplit(end$callback_msg, split = " ")[[1]][1]) / iters) * iters_full) / 60) / 60, digits = 1)
  cat("Full run of", crayon::green$bold(iters_full), "iterations will take", crayon::red$bold$underline$italic(runtime), "hours", "\u2693","\n")
}

# write pkg data ----
if(isTRUE(write_data)){
  afscISS::pkg_data(append = FALSE)
}

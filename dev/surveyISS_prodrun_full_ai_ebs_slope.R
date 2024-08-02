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

# aleutian islands ----
data_ai <- data$data_ai

## run for all species (and subsetting out species cases so we don't have two places with those results) ----
data_ai$cpue %>%
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052))) -> .cpue
data_ai$lfreq %>%
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052))) -> .lfreq
data_ai$specimen %>%
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052))) -> .specimen
strata_data <- data_ai$strata

# age/length
surveyISS::srvy_iss(iters = iters,
                    lfreq_data = .lfreq,
                    specimen_data = .specimen,
                    cpue_data = .cpue,
                    strata_data = strata_data,
                    yrs = 1991,
                    boot_hauls = TRUE,
                    boot_lengths = TRUE,
                    boot_ages = TRUE,
                    al_var = TRUE,
                    al_var_ann = TRUE,
                    age_err = TRUE,
                    region = 'ai',
                    save_interm = TRUE,
                    save_stats = TRUE,
                    save = 'prod')

# caal
surveyISS::srvy_iss_caal(iters = iters,
                         specimen_data = .specimen,
                         cpue_data = .cpue,
                         yrs = 1991,
                         boot_hauls = TRUE,
                         boot_ages = TRUE,
                         al_var = TRUE,
                         al_var_ann = TRUE,
                         age_err = TRUE,
                         region = 'ai',
                         save_interm = TRUE,
                         save_stats = TRUE,
                         save = 'prod')

## run for ai subregion ----
surveyISS::srvy_iss_ai_subreg(iters = iters,
                              lfreq_data = .lfreq,
                              specimen_data = .specimen,
                              cpue_data = .cpue,
                              strata_data = strata_data,
                              yrs = 1991,
                              boot_hauls = TRUE,
                              boot_lengths = TRUE,
                              boot_ages = TRUE,
                              al_var = TRUE,
                              al_var_ann = TRUE,
                              age_err = TRUE,
                              region = 'ai',
                              save_interm = TRUE,
                              save_stats = TRUE,
                              save = 'prod')


## run for ai blackspotted-rougheye stock complex ----
data_ai$cpue %>%
  tidytable::filter(species_code %in% c(30050, 30051, 30052)) -> .cpue_bsre
data_ai$lfreq %>%
  tidytable::filter(species_code %in% c(30050, 30051, 30052)) -> .lfreq_bsre
data_ai$specimen %>%
  tidytable::filter(species_code %in% c(30050, 30051, 30052)) -> .specimen_bsre

surveyISS::srvy_iss_ai_cmplx(iters = iters,
                             lfreq_data = .lfreq_bsre,
                             specimen_data = .specimen_bsre,
                             cpue_data = .cpue_bsre,
                             strata_data = strata_data,
                             yrs = 1991,
                             boot_hauls = TRUE,
                             boot_lengths = TRUE,
                             boot_ages = TRUE,
                             al_var = TRUE,
                             al_var_ann = TRUE,
                             age_err = TRUE,
                             cmplx_code = 3005012,
                             cmplx = 'bsre',
                             region = 'ai',
                             save_interm = TRUE,
                             save_stats = TRUE,
                             save = 'prod')

# ebs slope ----
data_ebss <- data$data_ebss

# age/length
surveyISS::srvy_iss(iters = iters,
                    lfreq_data = data_ebss$lfreq,
                    specimen_data = data_ebss$specimen,
                    cpue_data = data_ebss$cpue,
                    strata_data = data_ebss$strata,
                    yrs = 2002,
                    boot_hauls = TRUE,
                    boot_lengths = TRUE,
                    boot_ages = TRUE,
                    al_var = TRUE,
                    al_var_ann = TRUE,
                    age_err = TRUE,
                    region = 'ebs_slope',
                    save_interm = TRUE,
                    save_stats = TRUE,
                    save = 'prod')

# caal
surveyISS::srvy_iss_caal(iters = iters, 
                         specimen_data = data_ebss$specimen, 
                         cpue_data = data_ebss$cpue, 
                         yrs = 2002,
                         boot_hauls = TRUE, 
                         boot_ages = TRUE,
                         al_var = TRUE, 
                         al_var_ann = TRUE, 
                         age_err = TRUE,
                         region = 'ebs_slope', 
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

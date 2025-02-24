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

# nbs & ebs ----
data_nebs <- data$data_nebs

# age/length
surveyISS::srvy_iss(iters = iters,
                    lfreq_data = data_nebs$lfreq,
                    specimen_data = data_nebs$specimen,
                    cpue_data = data_nebs$cpue,
                    strata_data = data_nebs$strata,
                    yrs = 1979,
                    boot_hauls = TRUE,
                    boot_lengths = TRUE,
                    boot_ages = TRUE,
                    al_var = TRUE,
                    al_var_ann = TRUE,
                    age_err = TRUE,
                    region = 'nebs',
                    save_interm = TRUE,
                    save_stats = TRUE,
                    save = 'prod')

# caal
surveyISS::srvy_iss_caal(iters = iters, 
                         specimen_data = data_nebs$specimen, 
                         cpue_data = data_nebs$cpue, 
                         yrs = 1979,
                         boot_hauls = TRUE, 
                         boot_ages = TRUE,
                         al_var = TRUE, 
                         al_var_ann = TRUE, 
                         age_err = TRUE,
                         region = 'nebs', 
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

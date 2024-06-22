#' function to get surveyISS results transferred to data package
#' 
#' @description
#' Function that retrieves composition ISS results for AFSC stock assessments
#' 
#' @param append boolean. whether to append results to previous results (default = TRUE)
#'
#' @return .rda files within /data folder
#' 
#' @export
#'
pkg_data <- function(append = TRUE) {
  
  # util fcn
  get_stats <- function(data, grp, column){
    data  %>% 
      tidytable::filter(!is.infinite({{column}})) %>% 
      tidytable::summarise(bs_med = median({{column}}, na.rm = TRUE),
                           bs_mu = mean({{column}}, na.rm = TRUE),
                           bs_sd = sd({{column}}, na.rm = TRUE),
                           q2_5th = quantile({{column}}, 0.025, na.rm = TRUE),
                           q25th = quantile({{column}}, 0.25, na.rm = TRUE),
                           q75th = quantile({{column}}, 0.75, na.rm = TRUE),
                           q97_5th = quantile({{column}}, 0.975, na.rm = TRUE),
                           .by = grp)
  }
  
  # pull surveyISS results ----
  ## ai results ----
  ai <- purrr::map(list.files(here::here('output', 'ai')), ~ vroom::vroom(here::here('output', 'ai', .)))
  names(ai) <- list.files(here::here('output', 'ai')) %>% 
    stringr::str_replace(., '.csv', "")
  
  ### iter'd summ stats ----
  
  #### comp data ----
  if(length(ai$prod_resampled_age) > 0){
    ai$prod_resampled_age <- get_stats(data = ai$prod_resampled_age,
                                       grp = c('year', 'species_code', 'sex', 'age'),
                                       column = agepop)
  }
  print('ai iterd comp 1 of 7 done')
  if(length(ai$prod_resampled_age_bsre) > 0){
    ai$prod_resampled_age_bsre <- get_stats(data = ai$prod_resampled_age_bsre,
                                            grp = c('year', 'species_code', 'sex', 'age'),
                                            column = agepop)
  }
  print('ai iterd comp 2 of 7 done')
  if(length(ai$prod_resampled_age_ai_subreg) > 0){
    ai$prod_resampled_age_ai_subreg <- get_stats(data = ai$prod_resampled_age_ai_subreg,
                                                 grp = c('year', 'region', 'species_code', 'sex', 'age'),
                                                 column = agepop)
  }
  print('ai iterd comp 3 of 7 done')
  if(length(ai$prod_resampled_length) > 0){
    ai$prod_resampled_length <- get_stats(data = ai$prod_resampled_length,
                                          grp = c('year', 'species_code', 'sex', 'length'),
                                          column = abund)
  }
  print('ai iterd comp 4 of 7 done')
  if(length(ai$prod_resampled_length_bsre) > 0){
    ai$prod_resampled_length_bsre <- get_stats(data = ai$prod_resampled_length_bsre,
                                               grp = c('year', 'species_code', 'sex', 'length'),
                                               column = abund)
  }
  print('ai iterd comp 5 of 7 done')
  if(length(ai$prod_resampled_length_ai_subreg) > 0){
    ai$prod_resampled_length_ai_subreg <- get_stats(data = ai$prod_resampled_length_ai_subreg,
                                                    grp = c('year', 'region', 'species_code', 'sex', 'length'),
                                                    column = abund)
  }
  print('ai iterd comp 6 of 7 done')
  if(length(ai$prod_resampled_caal) > 0){
    ai$prod_resampled_caal <- get_stats(data = ai$prod_resampled_caal,
                                        grp = c('year', 'species_code', 'sex', 'length', 'age'),
                                        column = caal)
  }
  print('ai iterd comp 7 of 7 done')
  
  #### rss ----
  if(length(ai$prod_iter_rss_ag) > 0){
    ai$prod_iter_rss_ag <- get_stats(data = ai$prod_iter_rss_ag,
                                     grp = c('year', 'species_code', 'sex', 'sex_desc'),
                                     column = rss)
  }
  print('ai iterd rss 1 of 7 done')
  if(length(ai$prod_iter_rss_ag_bsre) > 0){
    ai$prod_iter_rss_ag_bsre <- get_stats(data = ai$prod_iter_rss_ag_bsre,
                                          grp = c('year', 'species_code', 'sex', 'sex_desc'),
                                          column = rss)
  }
  print('ai iterd rss 2 of 7 done')
  if(length(ai$prod_iter_rss_ag_ai_subreg) > 0){
    ai$prod_iter_rss_ag_ai_subreg <- get_stats(data = ai$prod_iter_rss_ag_ai_subreg,
                                               grp = c('year', 'region', 'species_code', 'sex', 'sex_desc'),
                                               column = rss)
  }
  print('ai iterd rss 3 of 7 done')
  if(length(ai$prod_iter_rss_ln) > 0){
    ai$prod_iter_rss_ln <- get_stats(data = ai$prod_iter_rss_ln,
                                     grp = c('year', 'species_code', 'sex', 'sex_desc'),
                                     column = rss)
  }
  print('ai iterd rss 4 of 7 done')
  if(length(ai$prod_iter_rss_ln_bsre) > 0){
    ai$prod_iter_rss_ln_bsre <- get_stats(data = ai$prod_iter_rss_ln_bsre,
                                          grp = c('year', 'species_code', 'sex', 'sex_desc'),
                                          column = rss)
  }
  print('ai iterd rss 5 of 7 done')
  if(length(ai$prod_iter_rss_ln_ai_subreg) > 0){
    ai$prod_iter_rss_ln_ai_subreg <- get_stats(data = ai$prod_iter_rss_ln_ai_subreg,
                                               grp = c('year', 'region', 'species_code', 'sex', 'sex_desc'),
                                               column = rss)
  }
  print('ai iterd rss 6 of 7 done')
  if(length(ai$prod_iter_rss_caal) > 0){
    ai$prod_iter_rss_caal <- get_stats(data = ai$prod_iter_rss_caal,
                                       grp = c('year', 'species_code', 'sex', 'sex_desc', 'length'),
                                       column = rss)
  }
  print('ai iterd rss 7 of 7 done')
  
  ## ebs results ----
  ebs <- purrr::map(list.files(here::here('output', 'ebs')), ~ vroom::vroom(here::here('output', 'ebs', .)))
  names(ebs) <- list.files(here::here('output', 'ebs')) %>% 
    stringr::str_replace(., '.csv', "")
  
  ### iter'd summ stats ----
  
  #### comp data ----
  if(length(ebs$prod_resampled_age) > 0){
    ebs$prod_resampled_age <- get_stats(data = ebs$prod_resampled_age,
                                        grp = c('year', 'species_code', 'sex', 'age'),
                                        column = agepop)
  }
  print('ebs iterd comp 1 of 3 done')
  if(length(ebs$prod_resampled_length) > 0){
    ebs$prod_resampled_length <- get_stats(data = ebs$prod_resampled_length,
                                           grp = c('year', 'species_code', 'sex', 'length'),
                                           column = abund)
  }
  print('ebs iterd comp 2 of 3 done')
  if(length(ebs$prod_resampled_caal) > 0){
    ebs$prod_resampled_caal <- get_stats(data = ebs$prod_resampled_caal,
                                         grp = c('year', 'species_code', 'sex', 'length', 'age'),
                                         column = caal)
  }
  print('ebs iterd comp 3 of 3 done')
  
  #### rss ----
  if(length(ebs$prod_iter_rss_ag) > 0){
    ebs$prod_iter_rss_ag <- get_stats(data = ebs$prod_iter_rss_ag,
                                      grp = c('year', 'species_code', 'sex', 'sex_desc'),
                                      column = rss)
  }
  print('ebs iterd rss 1 of 3 done')
  if(length(ebs$prod_iter_rss_ln) > 0){
    ebs$prod_iter_rss_ln <- get_stats(data = ebs$prod_iter_rss_ln,
                                      grp = c('year', 'species_code', 'sex', 'sex_desc'),
                                      column = rss)
  }
  print('ebs iterd rss 2 of 3 done')
  if(length(ebs$prod_iter_rss_caal) > 0){
    ebs$prod_iter_rss_caal <- get_stats(data = ebs$prod_iter_rss_caal,
                                        grp = c('year', 'species_code', 'sex', 'sex_desc', 'length'),
                                        column = rss)
  }
  print('ebs iterd rss 3 of 3 done')
  
  ## ebs_slope results ----
  ebs_slope <- purrr::map(list.files(here::here('output', 'ebs_slope')), ~ vroom::vroom(here::here('output', 'ebs_slope', .)))
  names(ebs_slope) <- list.files(here::here('output', 'ebs_slope')) %>% 
    stringr::str_replace(., '.csv', "")
  
  ### iter'd summ stats ----
  
  #### comp data ----
  if(length(ebs_slope$prod_resampled_age) > 0){
    ebs_slope$prod_resampled_age <- get_stats(data = ebs_slope$prod_resampled_age,
                                              grp = c('year', 'species_code', 'sex', 'age'),
                                              column = agepop)
  }
  print('ebs_slope iterd comp 1 of 3 done')
  if(length(ebs_slope$prod_resampled_length) > 0){
    ebs_slope$prod_resampled_length <- get_stats(data = ebs_slope$prod_resampled_length,
                                                 grp = c('year', 'species_code', 'sex', 'length'),
                                                 column = abund)
  }
  print('ebs_slope iterd comp 2 of 3 done')
  if(length(ebs_slope$prod_resampled_caal) > 0){
    ebs_slope$prod_resampled_caal <- get_stats(data = ebs_slope$prod_resampled_caal,
                                               grp = c('year', 'species_code', 'sex', 'length', 'age'),
                                               column = caal)
  }
  print('ebs_slope iterd comp 3 of 3 done')
  
  #### rss ----
  if(length(ebs_slope$prod_iter_rss_ag) > 0){
    ebs_slope$prod_iter_rss_ag <- get_stats(data = ebs_slope$prod_iter_rss_ag,
                                            grp = c('year', 'species_code', 'sex', 'sex_desc'),
                                            column = rss)
  }
  print('ebs_slope iterd rss 1 of 3 done')
  if(length(ebs_slope$prod_iter_rss_ln) > 0){
    ebs_slope$prod_iter_rss_ln <- get_stats(data = ebs_slope$prod_iter_rss_ln,
                                            grp = c('year', 'species_code', 'sex', 'sex_desc'),
                                            column = rss)
  }
  print('ebs_slope iterd rss 2 of 3 done')
  if(length(ebs_slope$prod_iter_rss_caal) > 0){
    ebs_slope$prod_iter_rss_caal <- get_stats(data = ebs_slope$prod_iter_rss_caal,
                                              grp = c('year', 'species_code', 'sex', 'sex_desc', 'length'),
                                              column = rss)
  }
  print('ebs_slope iterd rss 3 of 3 done')
  
  ## goa results ----
  goa <- purrr::map(list.files(here::here('output', 'goa')), ~ vroom::vroom(here::here('output', 'goa', .)))
  names(goa) <- list.files(here::here('output', 'goa')) %>% 
    stringr::str_replace(., '.csv', "")
  
  ### iter'd summ stats ----
  
  #### comp data ----
  if(length(goa$prod_resampled_age) > 0){
    goa$prod_resampled_age <- get_stats(data = goa$prod_resampled_age,
                                        grp = c('year', 'species_code', 'sex', 'age'),
                                        column = agepop)
  }
  print('goa iterd comp 1 of 13 done')
  if(length(goa$prod_resampled_age_rebs) > 0){
    goa$prod_resampled_age_rebs <- get_stats(data = goa$prod_resampled_age_rebs,
                                             grp = c('year', 'species_code', 'sex', 'age'),
                                             column = agepop)
  }
  print('goa iterd comp 2 of 13 done')
  if(length(goa$prod_resampled_age_dr) > 0){
    goa$prod_resampled_age_dr <- get_stats(data = goa$prod_resampled_age_dr,
                                           grp = c('year', 'species_code', 'sex', 'age'),
                                           column = agepop)
  }
  print('goa iterd comp 3 of 13 done')
  if(length(goa$prod_resampled_age_w140) > 0){
    goa$prod_resampled_age_w140 <- get_stats(data = goa$prod_resampled_age_w140,
                                             grp = c('year', 'species_code', 'sex', 'age'),
                                             column = agepop)
  }
  print('goa iterd comp 4 of 13 done')
  if(length(goa$prod_resampled_age_wc_egoa) > 0){
    goa$prod_resampled_age_wc_egoa <- get_stats(data = goa$prod_resampled_age_wc_egoa,
                                                grp = c('year', 'region', 'species_code', 'sex', 'age'),
                                                column = agepop)
  }
  print('goa iterd comp 5 of 13 done')
  if(length(goa$prod_resampled_age_w_c_egoa) > 0){
    goa$prod_resampled_age_w_c_egoa <- get_stats(data = goa$prod_resampled_age_w_c_egoa,
                                                 grp = c('year', 'region', 'species_code', 'sex', 'age'),
                                                 column = agepop)
  }
  print('goa iterd comp 6 of 13 done')
  if(length(goa$prod_resampled_length) > 0){
    goa$prod_resampled_length <- get_stats(data = goa$prod_resampled_length,
                                           grp = c('year', 'species_code', 'sex', 'length'),
                                           column = abund)
  }
  print('goa iterd comp 7 of 13 done')
  if(length(goa$prod_resampled_length_rebs) > 0){
    goa$prod_resampled_length_rebs <- get_stats(data = goa$prod_resampled_length_rebs,
                                                grp = c('year', 'species_code', 'sex', 'length'),
                                                column = abund)
  }
  print('goa iterd comp 8 of 13 done')
  if(length(goa$prod_resampled_length_dr) > 0){
    goa$prod_resampled_length_dr <- get_stats(data = goa$prod_resampled_length_dr,
                                              grp = c('year', 'species_code', 'sex', 'length'),
                                              column = abund)
  }
  print('goa iterd comp 9 of 13 done')
  if(length(goa$prod_resampled_length_w140) > 0){
    goa$prod_resampled_length_w140 <- get_stats(data = goa$prod_resampled_length_w140,
                                                grp = c('year', 'species_code', 'sex', 'length'),
                                                column = abund)
  }
  print('goa iterd comp 10 of 13 done')
  if(length(goa$prod_resampled_length_wc_egoa) > 0){
    goa$prod_resampled_length_wc_egoa <- get_stats(data = goa$prod_resampled_length_wc_egoa,
                                                   grp = c('year', 'region', 'species_code', 'sex', 'length'),
                                                   column = abund)
  }
  print('goa iterd comp 11 of 13 done')
  if(length(goa$prod_resampled_length_w_c_egoa) > 0){
    goa$prod_resampled_length_w_c_egoa <- get_stats(data = goa$prod_resampled_length_w_c_egoa,
                                                    grp = c('year', 'region', 'species_code', 'sex', 'length'),
                                                    column = abund)
  }
  print('goa iterd comp 12 of 13 done')
  if(length(goa$prod_resampled_caal) > 0){
    goa$prod_resampled_caal <- get_stats(data = goa$prod_resampled_caal,
                                         grp = c('year', 'species_code', 'sex', 'length', 'age'),
                                         column = caal)
  }
  print('goa iterd comp 13 of 13 done')
  
  #### rss ----
  if(length(goa$prod_iter_rss_ag) > 0){
    goa$prod_iter_rss_ag <- get_stats(data = goa$prod_iter_rss_ag,
                                      grp = c('year', 'species_code', 'sex', 'sex_desc'),
                                      column = rss)
  }
  print('goa iterd rss 1 of 13 done')
  if(length(goa$prod_iter_rss_ag_rebs) > 0){
    goa$prod_iter_rss_ag_rebs <- get_stats(data = goa$prod_iter_rss_ag_rebs,
                                           grp = c('year', 'species_code', 'sex', 'sex_desc'),
                                           column = rss)
  }
  print('goa iterd rss 2 of 13 done')
  if(length(goa$prod_iter_rss_ag_dr) > 0){
    goa$prod_iter_rss_ag_dr <- get_stats(data = goa$prod_iter_rss_ag_dr,
                                         grp = c('year', 'species_code', 'sex', 'sex_desc'),
                                         column = rss)
  }
  print('goa iterd rss 3 of 13 done')
  if(length(goa$prod_iter_rss_ag_w140) > 0){
    goa$prod_iter_rss_ag_w140 <- get_stats(data = goa$prod_iter_rss_ag_w140,
                                           grp = c('year', 'species_code', 'sex', 'sex_desc'),
                                           column = rss)
  }
  print('goa iterd rss 4 of 13 done')
  if(length(goa$prod_iter_rss_ag_wc_egoa) > 0){
    goa$prod_iter_rss_ag_wc_egoa <- get_stats(data = goa$prod_iter_rss_ag_wc_egoa,
                                              grp = c('year', 'region', 'species_code', 'sex', 'sex_desc'),
                                              column = rss)
  }
  print('goa iterd rss 5 of 13 done')
  if(length(goa$prod_iter_rss_ag_w_c_egoa) > 0){
    goa$prod_iter_rss_ag_w_c_egoa <- get_stats(data = goa$prod_iter_rss_ag_w_c_egoa,
                                               grp = c('year', 'region', 'species_code', 'sex', 'sex_desc'),
                                               column = rss)
  }
  print('goa iterd rss 6 of 13 done')
  if(length(goa$prod_iter_rss_ln) > 0){
    goa$prod_iter_rss_ln <- get_stats(data = goa$prod_iter_rss_ln,
                                      grp = c('year', 'species_code', 'sex', 'sex_desc'),
                                      column = rss)
  }
  print('goa iterd rss 7 of 13 done')
  if(length(goa$prod_iter_rss_ln_rebs) > 0){
    goa$prod_iter_rss_ln_rebs <- get_stats(data = goa$prod_iter_rss_ln_rebs,
                                           grp = c('year', 'species_code', 'sex', 'sex_desc'),
                                           column = rss)
  }
  print('goa iterd rss 8 of 13 done')
  if(length(goa$prod_iter_rss_ln_dr) > 0){
    goa$prod_iter_rss_ln_dr <- get_stats(data = goa$prod_iter_rss_ln_dr,
                                         grp = c('year', 'species_code', 'sex', 'sex_desc'),
                                         column = rss)
  }
  print('goa iterd rss 9 of 13 done')
  if(length(goa$prod_iter_rss_ln_w140) > 0){
    goa$prod_iter_rss_ln_w140 <- get_stats(data = goa$prod_iter_rss_ln_w140,
                                           grp = c('year', 'species_code', 'sex', 'sex_desc'),
                                           column = rss)
  }
  print('goa iterd rss 10 of 13 done')
  if(length(goa$prod_iter_rss_ln_wc_egoa) > 0){
    goa$prod_iter_rss_ln_wc_egoa <- get_stats(data = goa$prod_iter_rss_ln_wc_egoa,
                                              grp = c('year', 'region', 'species_code', 'sex', 'sex_desc'),
                                              column = rss)
  }
  print('goa iterd rss 11 of 13 done')
  if(length(goa$prod_iter_rss_ln_w_c_egoa) > 0){
    goa$prod_iter_rss_ln_w_c_egoa <- get_stats(data = goa$prod_iter_rss_ln_w_c_egoa,
                                               grp = c('year', 'region', 'species_code', 'sex', 'sex_desc'),
                                               column = rss)
  }
  print('goa iterd rss 12 of 13 done')
  if(length(goa$prod_iter_rss_caal) > 0){
    goa$prod_iter_rss_caal <- get_stats(data = goa$prod_iter_rss_caal,
                                        grp = c('year', 'species_code', 'sex', 'sex_desc', 'length'),
                                        column = rss)
  }
  print('goa iterd rss 13 of 13 done')
  
  ## nebs results ----
  nebs <- purrr::map(list.files(here::here('output', 'nebs')), ~ vroom::vroom(here::here('output', 'nebs', .)))
  names(nebs) <- list.files(here::here('output', 'nebs')) %>% 
    stringr::str_replace(., '.csv', "")
  
  ### iter'd summ stats ----
  
  #### comp data ----
  if(length(nebs$prod_resampled_age) > 0){
    nebs$prod_resampled_age <- get_stats(data = nebs$prod_resampled_age,
                                         grp = c('year', 'species_code', 'sex', 'age'),
                                         column = agepop)
  }
  print('nebs iterd comp 1 of 3 done')
  if(length(nebs$prod_resampled_length) > 0){
    nebs$prod_resampled_length <- get_stats(data = nebs$prod_resampled_length,
                                            grp = c('year', 'species_code', 'sex', 'length'),
                                            column = abund)
  }
  print('nebs iterd comp 2 of 3 done')
  if(length(nebs$prod_resampled_caal) > 0){
    nebs$prod_resampled_caal <- get_stats(data = nebs$prod_resampled_caal,
                                          grp = c('year', 'species_code', 'sex', 'length', 'age'),
                                          column = caal)
  }
  print('nebs iterd comp 3 of 3 done')
  
  #### rss ----
  if(length(nebs$prod_iter_rss_ag) > 0){
    nebs$prod_iter_rss_ag <- get_stats(data = nebs$prod_iter_rss_ag,
                                       grp = c('year', 'species_code', 'sex', 'sex_desc'),
                                       column = rss)
  }
  print('nebs iterd rss 1 of 3 done')
  if(length(nebs$prod_iter_rss_ln) > 0){
    nebs$prod_iter_rss_ln <- get_stats(data = nebs$prod_iter_rss_ln,
                                       grp = c('year', 'species_code', 'sex', 'sex_desc'),
                                       column = rss)
  }
  print('nebs iterd rss 2 of 3 done')
  if(length(nebs$prod_iter_rss_caal) > 0){
    nebs$prod_iter_rss_caal <- get_stats(data = nebs$prod_iter_rss_caal,
                                         grp = c('year', 'species_code', 'sex', 'sex_desc', 'length'),
                                         column = rss)
  }
  print('nebs iterd rss 3 of 3 done')
  
  # when appending a year to pre-existing data ----
  if(isTRUE(append)){

    ## write out results as package data ----
    
    data_old <- load(here::here('data', 'data_iss.rda'))
    
    
    
    data_iss <- list(ai, ebs, ebs_slope, goa, nebs)
    names(data_iss) <- c('ai', 'ebs', 'ebs_slope', 'goa', 'nebs')
    
    
    
    
    
    
    
    
    usethis::use_data(data_iss, overwrite = TRUE)
    
    
    

    
    
  } else{
    # when full run occurs ----
       
    ## write out results as package data ----
    
    data_iss <- list(ai, ebs, ebs_slope, goa, nebs)
    names(data_iss) <- c('ai', 'ebs', 'ebs_slope', 'goa', 'nebs')
    usethis::use_data(data_iss, overwrite = TRUE)
  }

}

#' Results from surveyISS package
#'
#' A list containing the various output dataframes from the surveyISS package
#'
#' @format A list
#' \describe{
#' A list of ISS, age/length pop'n numbers, bootstrap bias, mean length-at-age, replicated rss, replicated age/length pop'n numbers
#' }
"data_iss"

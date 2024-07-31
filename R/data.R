#' function to get surveyISS results transferred to data package
#' 
#' @description
#' Function that creates package data for afscISS (NOTE: this is not a user fcn, this is a developer/maintainer fcn)
#' 
#' @param append boolean. whether to append results to previous results (default = TRUE)
#'
#' @return .rda files within /data folder
#' 
#' @export
#'
pkg_data <- function(append = TRUE) {

  # pull surveyISS results ----
  ## ai results ----
  if(length(list.files(here::here('output', 'ai'))) > 0){
  ai <- purrr::map(list.files(here::here('output', 'ai')), ~ vroom::vroom(here::here('output', 'ai', .)))
  ai <- purrr::map(1:length(ai), ~ summ_stats(ai[[.]], ., length(ai), 'ai'))
  names(ai) <- list.files(here::here('output', 'ai')) %>% 
    stringr::str_replace(., '.csv', "")
  } else{ai <- NULL}
  
  
  ai_base <- purrr::map(list.files(here::here('output', 'ai'), pattern = "prod_base"), ~ vroom::vroom(here::here('output', 'ai', .)))

  ai_resamp <- purrr::map(list.files(here::here('output', 'ai'), pattern = "prod_resampled"), ~ vroom::vroom(here::here('output', 'ai', .)))
  
  
  ai_comps <- purrr::map(1:length(ai_base), ~ get_comps(ai_base[[.]], ai_resamp[[.]], ., length(ai_base), 'ai'))
  
  
  
  names(ai_comps) <- list.files(here::here('output', 'ai'), pattern = "prod_base") %>% 
    stringr::str_replace(., '.csv', "") %>% 
    stringr::str_replace(., 'base', "comp")
  


  
  
  
  
  ## ebs results ----
  if(length(list.files(here::here('output', 'ebs'))) > 0){
  ebs <- purrr::map(list.files(here::here('output', 'ebs')), ~ vroom::vroom(here::here('output', 'ebs', .)))
  ebs <- purrr::map(1:length(ebs), ~ summ_stats(ebs[[.]], ., length(ebs), 'ebs'))
  names(ebs) <- list.files(here::here('output', 'ebs')) %>% 
    stringr::str_replace(., '.csv', "")
  } else{ebs <- NULL}

  ## ebs_slope results ----
  if(length(list.files(here::here('output', 'ebs_slope'))) > 0){
  ebs_slope <- purrr::map(list.files(here::here('output', 'ebs_slope')), ~ vroom::vroom(here::here('output', 'ebs_slope', .)))
  ebs_slope <- purrr::map(1:length(ebs_slope), ~ summ_stats(ebs_slope[[.]], ., length(ebs_slope), 'ebs_slope'))
  names(ebs_slope) <- list.files(here::here('output', 'ebs_slope')) %>% 
    stringr::str_replace(., '.csv', "")
  } else{ebs_slope <- NULL}
  
  ## goa results ----
  if(length(list.files(here::here('output', 'goa'))) > 0){
  goa <- purrr::map(list.files(here::here('output', 'goa')), ~ vroom::vroom(here::here('output', 'goa', .)))
  goa <- purrr::map(1:length(goa), ~ summ_stats(goa[[.]], ., length(goa), 'goa'))
  names(goa) <- list.files(here::here('output', 'goa')) %>% 
    stringr::str_replace(., '.csv', "")
  } else{goa <- NULL}
  
  ## nebs results ----
  if(length(list.files(here::here('output', 'nebs'))) > 0){
  nebs <- purrr::map(list.files(here::here('output', 'nebs')), ~ vroom::vroom(here::here('output', 'nebs', .)))
  nebs <- purrr::map(1:length(nebs), ~ summ_stats(nebs[[.]], ., length(nebs), 'nebs'))
  names(nebs) <- list.files(here::here('output', 'nebs')) %>% 
    stringr::str_replace(., '.csv', "")
  } else{nebs <- NULL}

  # when appending a year to pre-existing data ----
  if(isTRUE(append)){

    # util fcn
    bind_new <- function(data_new, data_old){
      data_old %>% 
        tidytable::bind_rows(data_new)
    }
    
    ## load old results ----
    load(file = here::here('data', 'data_iss.rda'))

    ## append ai ----
    if(length(ai) > 0){
      ai_new <- purrr::map(1:length(data_iss$ai),
                           ~ bind_new(ai[[names(data_iss$ai)[.]]], 
                                      data_iss$ai[[names(data_iss$ai)[.]]]))
      names(ai_new) <- names(data_iss$ai)
    } else{ai_new <- data_iss$ai}

    ## append ebs ----
    if(length(ebs) > 0){
      ebs_new <- purrr::map(1:length(data_iss$ebs),
                            ~ bind_new(ebs[[names(data_iss$ebs)[.]]], 
                                       data_iss$ebs[[names(data_iss$ebs)[.]]]))
      names(ebs_new) <- names(data_iss$ebs)
    } else{ebs_new <- data_iss$ebs}
    
    ## append ebs_slope ----
    if(length(ebs_slope) > 0){
      ebs_slope_new <- purrr::map(1:length(data_iss$ebs_slope),
                                  ~ bind_new(ebs_slope[[names(data_iss$ebs_slope)[.]]], 
                                             data_iss$ebs_slope[[names(data_iss$ebs_slope)[.]]]))
      names(ebs_slope_new) <- names(data_iss$ebs_slope)
    } else{ebs_slope_new <- data_iss$ebs_slope}
    
    ## append goa ----
    if(length(goa) > 0){
      goa_new <- purrr::map(1:length(data_iss$goa),
                            ~ bind_new(goa[[names(data_iss$goa)[.]]], 
                                       data_iss$goa[[names(data_iss$goa)[.]]]))
      names(goa_new) <- names(data_iss$goa)
    } else{goa_new <- data_iss$goa}
    
    ## append nebs ----
    if(length(nebs) > 0){
      nebs_new <- purrr::map(1:length(data_iss$nebs),
                             ~ bind_new(nebs[[names(data_iss$nebs)[.]]], 
                                        data_iss$nebs[[names(data_iss$nebs)[.]]]))
      names(nebs_new) <- names(data_iss$nebs)
    } else{nebs_new <- data_iss$nebs}
    
    ## write out results as package data ----
    data_iss <- list(ai_new, ebs_new, ebs_slope_new, goa_new, nebs_new)
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

#' function to summarize bootstrap results for composition data
#' 
#' @description
#' Function that computes mean and percentiles for iterated composition results from the surveyISS package (NOTE: this is not a user fcn, this is a developer/maintainer fcn)
#' 
#' @param data dataframe to be summarized
#' @param grp grouping for summarization
#' @param column column name to be summarized across iterations
#' @param dgts number of significant digits in rounding results
#'
#' @return dataframe with summary bootstrap statistics
#' 
#' @export
#'
get_comp_stats <- function(data, grp, column, dgts){
  data  %>% 
    tidytable::filter(!is.infinite({{column}})) %>% 
    tidytable::summarise(bs_mean = round(mean({{column}}, na.rm = TRUE), digits = dgts),
                         q2_5th = round(quantile({{column}}, 0.025, na.rm = TRUE), digits = dgts),
                         q97_5th = round(quantile({{column}}, 0.975, na.rm = TRUE), digits = dgts),
                         .by = grp)
}


#' function to get composition data
#' 
#' @description
#' Function that computes age/length/conditional age-at-length composition with bootstrap 95% simulation intervals
#' 
#' @param base_data base dataframe without resampling
#' @param res_data resampled dataframe
#' @param iter which dataframe is being summarized (for print message)
#' @param tot total number of dataframes being summarized (for print message)
#' @param reg survey region in which dataframe is being summarized (for print message)
#'
#' @return dataframe with summary bootstrap statistics
#' 
#' @export
#'
get_comps <- function(base_data, res_data, iter, tot, reg){

    # for length comps
    if('abund' %in% colnames(res_data)){
      # with subregion case
      if('region' %in% colnames(res_data)){
        # get base comps
        # compute comps for sex categories 0, 1, and 2
        base_data %>% 
          tidytable::filter(sex %in% c(0, 1, 2)) %>% 
          tidytable::mutate(tot = sum(abund), .by = c(year, region, species_code, sex)) %>% 
          tidytable::mutate(prop = abund / tot,
                            sex_c = sex) %>% 
          tidytable::select(year, region, species_code, sex, sex_c, length, prop) %>% 
          # compute comps for sex category 12
          tidytable::bind_rows(base_data %>% 
                                 tidytable::filter(sex %in% c(1, 2)) %>% 
                                 tidytable::mutate(tot = sum(abund), .by = c(year, region, species_code)) %>% 
                                 tidytable::mutate(prop = abund / tot,
                                                   sex_c = 12) %>% 
                                 tidytable::select(year, region, species_code, sex, sex_c, length, prop)) %>% 
          # compute comps for sex category 4
          tidytable::bind_rows(base_data %>% 
                                 tidytable::filter(sex != 0) %>% 
                                 tidytable::summarise(abund = sum(abund), .by = c(year, region, species_code, length)) %>% 
                                 tidytable::mutate(tot = sum(abund), .by = c(year, region, species_code)) %>% 
                                 tidytable::mutate(prop = abund / tot,
                                                   sex = 4,
                                                   sex_c = 4) %>% 
                                 tidytable::select(year, region, species_code, sex, sex_c, length, prop)) -> base_data1
        # get resampled comps
        # compute comps for sex categories 0, 1, and 2
        res_data %>% 
          tidytable::filter(sex %in% c(0, 1, 2)) %>% 
          tidytable::mutate(tot = sum(abund), .by = c(sim, year, region, species_code, sex)) %>% 
          tidytable::mutate(prop = abund / tot,
                            sex_c = sex) %>% 
          tidytable::select(sim, year, region, species_code, sex, sex_c, length, prop) %>% 
          # compute comps for sex category 12
          tidytable::bind_rows(res_data %>% 
                                 tidytable::filter(sex %in% c(1, 2)) %>% 
                                 tidytable::mutate(tot = sum(abund), .by = c(sim, year, region, species_code)) %>% 
                                 tidytable::mutate(prop = abund / tot,
                                                   sex_c = 12) %>% 
                                 tidytable::select(sim, year, region, species_code, sex, sex_c, length, prop)) %>% 
          # compute comps for sex category 4
          tidytable::bind_rows(res_data %>% 
                                 tidytable::filter(sex != 0) %>% 
                                 tidytable::summarise(abund = sum(abund), .by = c(sim, year, region, species_code, length)) %>% 
                                 tidytable::mutate(tot = sum(abund), .by = c(sim, year, region, species_code)) %>% 
                                 tidytable::mutate(prop = abund / tot,
                                                   sex = 4,
                                                   sex_c = 4) %>% 
                                 tidytable::select(sim, year, region, species_code, sex, sex_c, length, prop)) -> res_data1
        # get bs stats
        summ <- get_comp_stats(data = res_data1,
                               grp = c('year', 'region', 'species_code', 'sex', 'sex_c', 'length'),
                               column = prop,
                               dgts = 4)
        # join comp data with simulation intervals
        base_data1 %>% 
          tidytable::left_join(summ) %>% 
          # shift by bs bias
          tidytable::mutate(q2_5th = q2_5th + (prop - bs_mean),
                            q97_5th = q97_5th + (prop - bs_mean)) %>% 
          # replace lci < 0 with 0 and uci > 1 with 1
          tidytable::mutate(q2_5th = tidytable::case_when(q2_5th < 0 ~ 0,
                                                          .default = q2_5th),
                            q97_5th = tidytable::case_when(q97_5th > 1 ~ 1,
                                                           .default = q97_5th)) %>% 
          # rename columns
          tidytable::rename(lci = q2_5th, uci = q97_5th) %>% 
          tidytable::select(-bs_mean) -> comps
      } else{ # without subregion case
        # get base comps
        # compute comps for sex categories 0, 1, and 2
        base_data %>% 
          tidytable::filter(sex %in% c(0, 1, 2)) %>% 
          tidytable::mutate(tot = sum(abund), .by = c(year, species_code, sex)) %>% 
          tidytable::mutate(prop = abund / tot,
                            sex_c = sex) %>% 
          tidytable::select(year, species_code, sex, sex_c, length, prop) %>% 
          # compute comps for sex category 12
          tidytable::bind_rows(base_data %>% 
                                 tidytable::filter(sex %in% c(1, 2)) %>% 
                                 tidytable::mutate(tot = sum(abund), .by = c(year, species_code)) %>% 
                                 tidytable::mutate(prop = abund / tot,
                                                   sex_c = 12) %>% 
                                 tidytable::select(year, species_code, sex, sex_c, length, prop)) %>% 
          # compute comps for sex category 4
          tidytable::bind_rows(base_data %>% 
                                 tidytable::filter(sex != 0) %>% 
                                 tidytable::summarise(abund = sum(abund), .by = c(year, species_code, length)) %>% 
                                 tidytable::mutate(tot = sum(abund), .by = c(year, species_code)) %>% 
                                 tidytable::mutate(prop = abund / tot,
                                                   sex = 4,
                                                   sex_c = 4) %>% 
                                 tidytable::select(year,  species_code, sex, sex_c, length, prop)) -> base_data1
        # get resampled comps
        # compute comps for sex categories 0, 1, and 2
        res_data %>% 
          tidytable::filter(sex %in% c(0, 1, 2)) %>% 
          tidytable::mutate(tot = sum(abund), .by = c(sim, year, species_code, sex)) %>% 
          tidytable::mutate(prop = abund / tot,
                            sex_c = sex) %>% 
          tidytable::select(sim, year, species_code, sex, sex_c, length, prop) %>% 
          # compute comps for sex category 12
          tidytable::bind_rows(res_data %>% 
                                 tidytable::filter(sex %in% c(1, 2)) %>% 
                                 tidytable::mutate(tot = sum(abund), .by = c(sim, year, species_code)) %>% 
                                 tidytable::mutate(prop = abund / tot,
                                                   sex_c = 12) %>% 
                                 tidytable::select(sim, year, species_code, sex, sex_c, length, prop)) %>% 
          # compute comps for sex category 4
          tidytable::bind_rows(res_data %>% 
                                 tidytable::filter(sex != 0) %>% 
                                 tidytable::summarise(abund = sum(abund), .by = c(sim, year, species_code, length)) %>% 
                                 tidytable::mutate(tot = sum(abund), .by = c(sim, year, species_code)) %>% 
                                 tidytable::mutate(prop = abund / tot,
                                                   sex = 4,
                                                   sex_c = 4) %>% 
                                 tidytable::select(sim, year, species_code, sex, sex_c, length, prop)) -> res_data1
        # get bs stats
        summ <- get_comp_stats(data = res_data1,
                               grp = c('year', 'species_code', 'sex', 'sex_c', 'length'),
                               column = prop,
                               dgts = 4)
        # join comp data with simulation intervals
        base_data1 %>% 
          tidytable::left_join(summ) %>% 
          # shift by bs bias
          tidytable::mutate(q2_5th = q2_5th + (prop - bs_mean),
                            q97_5th = q97_5th + (prop - bs_mean)) %>% 
          # replace lci < 0 with 0 and uci > 1 with 1
          tidytable::mutate(q2_5th = tidytable::case_when(q2_5th < 0 ~ 0,
                                                          .default = q2_5th),
                            q97_5th = tidytable::case_when(q97_5th > 1 ~ 1,
                                                           .default = q97_5th)) %>% 
          # rename columns
          tidytable::rename(lci = q2_5th, uci = q97_5th) %>% 
          tidytable::select(-bs_mean) -> comps
      }
    }
  
    # for age comps
    if('agepop' %in% colnames(res_data)){
      # with subregion case
      if('region' %in% colnames(res_data)){
        # get base comps
        # compute comps for sex categories 0, 1, and 2
        base_data %>% 
          tidytable::filter(sex %in% c(0, 1, 2)) %>% 
          tidytable::mutate(tot = sum(agepop), .by = c(year, region, species_code, sex)) %>% 
          tidytable::mutate(prop = agepop / tot,
                            sex_c = sex) %>% 
          tidytable::select(year, region, species_code, sex, sex_c, age, prop) %>% 
          # compute comps for sex category 12
          tidytable::bind_rows(base_data %>% 
                                 tidytable::filter(sex %in% c(1, 2)) %>% 
                                 tidytable::mutate(tot = sum(agepop), .by = c(year, region, species_code)) %>% 
                                 tidytable::mutate(prop = agepop / tot,
                                                   sex_c = 12) %>% 
                                 tidytable::select(year, region, species_code, sex, sex_c, age, prop)) %>% 
          # compute comps for sex category 4
          tidytable::bind_rows(base_data %>% 
                                 tidytable::filter(sex != 0) %>% 
                                 tidytable::summarise(agepop = sum(agepop), .by = c(year, region, species_code, age)) %>% 
                                 tidytable::mutate(tot = sum(agepop), .by = c(year, region, species_code)) %>% 
                                 tidytable::mutate(prop = agepop / tot,
                                                   sex = 4,
                                                   sex_c = 4) %>% 
                                 tidytable::select(year, region, species_code, sex, sex_c, age, prop)) -> base_data1
        # get resampled comps
        # compute comps for sex categories 0, 1, and 2
        res_data %>% 
          tidytable::filter(sex %in% c(0, 1, 2)) %>% 
          tidytable::mutate(tot = sum(agepop), .by = c(sim, year, region, species_code, sex)) %>% 
          tidytable::mutate(prop = agepop / tot,
                            sex_c = sex) %>% 
          tidytable::select(sim, year, region, species_code, sex, sex_c, age, prop) %>% 
          # compute comps for sex category 12
          tidytable::bind_rows(res_data %>% 
                                 tidytable::filter(sex %in% c(1, 2)) %>% 
                                 tidytable::mutate(tot = sum(agepop), .by = c(sim, year, region, species_code)) %>% 
                                 tidytable::mutate(prop = agepop / tot,
                                                   sex_c = 12) %>% 
                                 tidytable::select(sim, year, region, species_code, sex, sex_c, age, prop)) %>% 
          # compute comps for sex category 4
          tidytable::bind_rows(res_data %>% 
                                 tidytable::filter(sex != 0) %>% 
                                 tidytable::summarise(agepop = sum(agepop), .by = c(sim, year, region, species_code, age)) %>% 
                                 tidytable::mutate(tot = sum(agepop), .by = c(sim, year, region, species_code)) %>% 
                                 tidytable::mutate(prop = agepop / tot,
                                                   sex = 4,
                                                   sex_c = 4) %>% 
                                 tidytable::select(sim, year, region, species_code, sex, sex_c, age, prop)) -> res_data1
        # get bs stats
        summ <- get_comp_stats(data = res_data1,
                               grp = c('year', 'region', 'species_code', 'sex', 'sex_c', 'age'),
                               column = prop,
                               dgts = 4)
        # join comp data with simulation intervals
        base_data1 %>% 
          tidytable::left_join(summ) %>% 
          # shift by bs bias
          tidytable::mutate(q2_5th = q2_5th + (prop - bs_mean),
                            q97_5th = q97_5th + (prop - bs_mean)) %>% 
          # replace lci < 0 with 0 and uci > 1 with 1
          tidytable::mutate(q2_5th = tidytable::case_when(q2_5th < 0 ~ 0,
                                                          .default = q2_5th),
                            q97_5th = tidytable::case_when(q97_5th > 1 ~ 1,
                                                           .default = q97_5th)) %>% 
          # rename columns
          tidytable::rename(lci = q2_5th, uci = q97_5th) %>% 
          tidytable::select(-bs_mean) -> comps
      } else{ # without subregion case
        # get base comps
        # compute comps for sex categories 0, 1, and 2
        base_data %>% 
          tidytable::filter(sex %in% c(0, 1, 2)) %>% 
          tidytable::mutate(tot = sum(agepop), .by = c(year, species_code, sex)) %>% 
          tidytable::mutate(prop = agepop / tot,
                            sex_c = sex) %>% 
          tidytable::select(year, species_code, sex, sex_c, age, prop) %>% 
          # compute comps for sex category 12
          tidytable::bind_rows(base_data %>% 
                                 tidytable::filter(sex %in% c(1, 2)) %>% 
                                 tidytable::mutate(tot = sum(agepop), .by = c(year, species_code)) %>% 
                                 tidytable::mutate(prop = agepop / tot,
                                                   sex_c = 12) %>% 
                                 tidytable::select(year, species_code, sex, sex_c, age, prop)) %>% 
          # compute comps for sex category 4
          tidytable::bind_rows(base_data %>% 
                                 tidytable::filter(sex != 0) %>% 
                                 tidytable::summarise(agepop = sum(agepop), .by = c(year, species_code, age)) %>% 
                                 tidytable::mutate(tot = sum(agepop), .by = c(year, species_code)) %>% 
                                 tidytable::mutate(prop = agepop / tot,
                                                   sex = 4,
                                                   sex_c = 4) %>% 
                                 tidytable::select(year, species_code, sex, sex_c, age, prop)) -> base_data1
        # get resampled comps
        # compute comps for sex categories 0, 1, and 2
        res_data %>% 
          tidytable::filter(sex %in% c(0, 1, 2)) %>% 
          tidytable::mutate(tot = sum(agepop), .by = c(sim, year, species_code, sex)) %>% 
          tidytable::mutate(prop = agepop / tot,
                            sex_c = sex) %>% 
          tidytable::select(sim, year, species_code, sex, sex_c, age, prop) %>% 
          # compute comps for sex category 12
          tidytable::bind_rows(res_data %>% 
                                 tidytable::filter(sex %in% c(1, 2)) %>% 
                                 tidytable::mutate(tot = sum(agepop), .by = c(sim, year, species_code)) %>% 
                                 tidytable::mutate(prop = agepop / tot,
                                                   sex_c = 12) %>% 
                                 tidytable::select(sim, year, species_code, sex, sex_c, age, prop)) %>% 
          # compute comps for sex category 4
          tidytable::bind_rows(res_data %>% 
                                 tidytable::filter(sex != 0) %>% 
                                 tidytable::summarise(agepop = sum(agepop), .by = c(sim, year, species_code, age)) %>% 
                                 tidytable::mutate(tot = sum(agepop), .by = c(sim, year, species_code)) %>% 
                                 tidytable::mutate(prop = agepop / tot,
                                                   sex = 4,
                                                   sex_c = 4) %>% 
                                 tidytable::select(sim, year, species_code, sex, sex_c, age, prop)) -> res_data1
        # get bs stats
        summ <- get_comp_stats(data = res_data1,
                               grp = c('year', 'species_code', 'sex', 'sex_c', 'age'),
                               column = prop,
                               dgts = 4)
        # join comp data with simulation intervals
        base_data1 %>% 
          tidytable::left_join(summ) %>% 
          # shift by bs bias
          tidytable::mutate(q2_5th = q2_5th + (prop - bs_mean),
                            q97_5th = q97_5th + (prop - bs_mean)) %>% 
          # replace lci < 0 with 0 and uci > 1 with 1
          tidytable::mutate(q2_5th = tidytable::case_when(q2_5th < 0 ~ 0,
                                                          .default = q2_5th),
                            q97_5th = tidytable::case_when(q97_5th > 1 ~ 1,
                                                           .default = q97_5th)) %>% 
          # rename columns
          tidytable::rename(lci = q2_5th, uci = q97_5th) %>% 
          tidytable::select(-bs_mean) -> comps
      }
    }
  
    # for caal
    if('caal' %in% colnames(res_data)){
      # get bs stats
      summ <- get_comp_stats(data = res_data,
                             grp = c('year', 'species_code', 'sex', 'length', 'age'),
                             column = caal,
                             dgts = 4)
      # join comp data with simulation intervals
      base_data %>% 
        tidytable::left_join(summ) %>% 
        # shift by bs bias
        tidytable::mutate(q2_5th = q2_5th + (caal - bs_mean),
                          q97_5th = q97_5th + (caal - bs_mean)) %>% 
        filter(caal != 1)
        # replace lci < 0 with 0 and uci > 1 with 1
        tidytable::mutate(q2_5th = tidytable::case_when(q2_5th < 0 ~ 0,
                                                        .default = q2_5th),
                          q97_5th = tidytable::case_when(q97_5th > 1 ~ 1,
                                                        .default = q97_5th)) %>% 
        # rename columns
        tidytable::rename(lci = q2_5th, uci = q97_5th) %>% 
        tidytable::select(-bs_mean) -> comps
      
    }

  cat(crayon::yellow(iter), "of", crayon::green(tot), crayon::blue$bold$underline(toupper(reg)), "comps objects summarized", crayon::green$bold("\u2713"), "\n")
  summ
  
}


















































#' wrapper function to perform summary statistics
#' 
#' @description
#' Function that tests whether summary statistics should be performed on results, and computes statistics with get_stats function (NOTE: this is not a user fcn, this is a developer/maintainer fcn)
#' 
#' @param data dataframe to be summarized
#' @param iter which dataframe is being summarized (for print message)
#' @param tot total number of dataframes being summarized (for print message)
#' @param reg survey region in which dataframe is being summarized (for print message)
#'
#' @return dataframe with summary bootstrap statistics
#' 
#' @export
#'
summ_stats <- function(data, iter, tot, reg){
  
  if('sim' %in% colnames(data)){
    
    # for comp results
    # for length comps
    if('abund' %in% colnames(data)){
      # with subregion case
      if('region' %in% colnames(data)){
        # compute comps for sex categories 0, 1, and 2
        data %>% 
          tidytable::filter(sex %in% c(0, 1, 2)) %>% 
          tidytable::mutate(tot = sum(abund), .by = c(sim, region, year, species_code, sex)) %>% 
          tidytable::mutate(prop = abund / tot,
                            sex_c = sex) %>% 
          tidytable::select(sim, region, year, species_code, sex, sex_c, length, prop) %>% 
          # compute comps for sex category 12
          tidytable::bind_rows(data %>% 
                                 tidytable::filter(sex %in% c(1, 2)) %>% 
                                 tidytable::mutate(tot = sum(abund), .by = c(sim, region, year, species_code)) %>% 
                                 tidytable::mutate(prop = abund / tot,
                                                   sex_c = 12) %>% 
                                 tidytable::select(sim, region, year, species_code, sex, sex_c, length, prop)) %>% 
          # compute comps for sex category 4
          tidytable::bind_rows(data %>% 
                                 tidytable::filter(sex != 0) %>% 
                                 tidytable::summarise(abund = sum(abund), .by = c(sim, region, year, species_code, length)) %>% 
                                 tidytable::mutate(tot = sum(abund), .by = c(sim, region, year, species_code)) %>% 
                                 tidytable::mutate(prop = abund / tot,
                                                   sex = 4,
                                                   sex_c = 4) %>% 
                                 tidytable::select(sim, region, year, species_code, sex, sex_c, length, prop)) -> data
        # get bs stats
        summ <- get_stats(data = data,
                          grp = c('year', 'region', 'species_code', 'sex', 'sex_c', 'length'),
                          column = prop,
                          dgts = 4)
      } else{ # without subregion case
        # compute comps for sex categories 0, 1, and 2
        data %>% 
          tidytable::filter(sex %in% c(0, 1, 2)) %>% 
          tidytable::mutate(tot = sum(abund), .by = c(sim, year, species_code, sex)) %>% 
          tidytable::mutate(prop = abund / tot,
                            sex_c = sex) %>% 
          tidytable::select(sim, year, species_code, sex, sex_c, length, prop) %>% 
          # compute comps for sex category 12
          tidytable::bind_rows(data %>% 
                                 tidytable::filter(sex %in% c(1, 2)) %>% 
                                 tidytable::mutate(tot = sum(abund), .by = c(sim, year, species_code)) %>% 
                                 tidytable::mutate(prop = abund / tot,
                                                   sex_c = 12) %>% 
                                 tidytable::select(sim, year, species_code, sex, sex_c, length, prop)) %>% 
          # compute comps for sex category 4
          tidytable::bind_rows(data %>% 
                                 tidytable::filter(sex != 0) %>% 
                                 tidytable::summarise(abund = sum(abund), .by = c(sim, year, species_code, length)) %>% 
                                 tidytable::mutate(tot = sum(abund), .by = c(sim, year, species_code)) %>% 
                                 tidytable::mutate(prop = abund / tot,
                                                   sex = 4,
                                                   sex_c = 4) %>% 
                                 tidytable::select(sim, year, species_code, sex, sex_c, length, prop)) -> data
        # get bs stats
        summ <- get_stats(data = data,
                          grp = c('year', 'species_code', 'sex', 'sex_c', 'length'),
                          column = prop,
                          dgts = 4)
      }
    }
    # for age comps
    if('agepop' %in% colnames(data)){
      # with subregion case
      if('region' %in% colnames(data)){
        # compute comps for sex categories 0, 1, and 2
        data %>% 
          tidytable::filter(sex %in% c(0, 1, 2)) %>% 
          tidytable::mutate(tot = sum(agepop), .by = c(sim, region, year, species_code, sex)) %>% 
          tidytable::mutate(prop = agepop / tot,
                            sex_c = sex) %>% 
          tidytable::select(sim, region, year, species_code, sex, sex_c, age, prop) %>% 
          # compute comps for sex category 12
          tidytable::bind_rows(data %>% 
                                 tidytable::filter(sex %in% c(1, 2)) %>% 
                                 tidytable::mutate(tot = sum(agepop), .by = c(sim, region, year, species_code)) %>% 
                                 tidytable::mutate(prop = agepop / tot,
                                                   sex_c = 12) %>% 
                                 tidytable::select(sim, region, year, species_code, sex, sex_c, age, prop)) %>% 
          # compute comps for sex category 4
          tidytable::bind_rows(data %>% 
                                 tidytable::filter(sex != 0) %>% 
                                 tidytable::summarise(agepop = sum(agepop), .by = c(sim, region, year, species_code, age)) %>% 
                                 tidytable::mutate(tot = sum(agepop), .by = c(sim, region, year, species_code)) %>% 
                                 tidytable::mutate(prop = agepop / tot,
                                                   sex = 4,
                                                   sex_c = 4) %>% 
                                 tidytable::select(sim, region, year, species_code, sex, sex_c, age, prop)) -> data
        # get bs stats
        summ <- get_stats(data = data,
                          grp = c('year', 'region', 'species_code', 'sex', 'sex_c', 'age'),
                          column = prop,
                          dgts = 4)
      } else{ # without subregion case
        # compute comps for sex categories 0, 1, and 2
        data %>% 
          tidytable::filter(sex %in% c(0, 1, 2)) %>% 
          tidytable::mutate(tot = sum(agepop), .by = c(sim, year, species_code, sex)) %>% 
          tidytable::mutate(prop = agepop / tot,
                            sex_c = sex) %>% 
          tidytable::select(sim, year, species_code, sex, sex_c, age, prop) %>% 
          # compute comps for sex category 12
          tidytable::bind_rows(data %>% 
                                 tidytable::filter(sex %in% c(1, 2)) %>% 
                                 tidytable::mutate(tot = sum(agepop), .by = c(sim, year, species_code)) %>% 
                                 tidytable::mutate(prop = agepop / tot,
                                                   sex_c = 12) %>% 
                                 tidytable::select(sim, year, species_code, sex, sex_c, age, prop)) %>% 
          # compute comps for sex category 4
          tidytable::bind_rows(data %>% 
                                 tidytable::filter(sex != 0) %>% 
                                 tidytable::summarise(agepop = sum(agepop), .by = c(sim, year, species_code, age)) %>% 
                                 tidytable::mutate(tot = sum(agepop), .by = c(sim, year, species_code)) %>% 
                                 tidytable::mutate(prop = agepop / tot,
                                                   sex = 4,
                                                   sex_c = 4) %>% 
                                 tidytable::select(sim, year, species_code, sex, sex_c, age, prop)) -> data
        # get bs stats
        summ <- get_stats(data = data,
                          grp = c('year', 'species_code', 'sex', 'sex_c', 'age'),
                          column = prop,
                          dgts = 4)
      }
    }
    # for caal
    if('caal' %in% colnames(data)){
      summ <- get_stats(data = data,
                        grp = c('year', 'species_code', 'sex', 'length', 'age'),
                        column = caal,
                        dgts = 4)
    }
    
    #for rss results
    if('rss' %in% colnames(data)){
      # with subregion case
      if('region' %in% colnames(data)){
        summ <- get_stats(data = data,
                          grp = c('year', 'region', 'species_code', 'sex', 'sex_desc'),
                          column = rss,
                          dgts = 0)
      } else{ # without subregion case
        summ <- get_stats(data = data,
                          grp = c('year', 'species_code', 'sex', 'sex_desc'),
                          column = rss,
                          dgts = 0)
      }
      # for caal
      if('length' %in% colnames(data)){
        summ <- get_stats(data = data,
                          grp = c('year', 'species_code', 'sex', 'sex_desc', 'length'),
                          column = rss,
                          dgts = 2)
      }
    }
  } else{
    summ <- data
  }
  
  cat(crayon::yellow(iter), "of", crayon::green(tot), crayon::blue$bold$underline(toupper(reg)), "objects summarized", crayon::green$bold("\u2713"), "\n")
  summ
  
}


















#' Results from surveyISS package
#'
#' A list containing the various output dataframes from the surveyISS package
#'
#' @format A list
#' \describe{
#' A list of ISS, age/length pop'n numbers, bootstrap bias, mean length-at-age, bootstrap summary statistics for replicated rss and age/length pop'n numbers
#' }
"data_iss"

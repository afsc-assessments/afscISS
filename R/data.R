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

#' function to summarize bootstrap results
#' 
#' @description
#' Function that computes mean and percentiles for iterated (comps and rss) results from the surveyISS package (NOTE: this is not a user fcn, this is a developer/maintainer fcn)
#' 
#' @param data dataframe to be summarized
#' @param grp grouping for summarization
#' @param column column name to be summarized across iterations
#'
#' @return dataframe with summary bootstrap statistics
#' 
#' @export
#'
get_stats <- function(data, grp, column){
  data  %>% 
    tidytable::filter(!is.infinite({{column}})) %>% 
    tidytable::summarise(bs_mean = mean({{column}}, na.rm = TRUE),
                         bs_sd = sd({{column}}, na.rm = TRUE),
                         q2_5th = quantile({{column}}, 0.025, na.rm = TRUE),
                         q25th = quantile({{column}}, 0.25, na.rm = TRUE),
                         q75th = quantile({{column}}, 0.75, na.rm = TRUE),
                         q97_5th = quantile({{column}}, 0.975, na.rm = TRUE),
                         .by = grp)
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
                            sex_cat = sex) %>% 
          tidytable::select(sim, region, year, species_code, sex, length, prop) %>% 
          # compute comps for sex category 12
          tidytable::bind_rows(data %>% 
                                 tidytable::filter(sex %in% c(1, 2)) %>% 
                                 tidytable::mutate(tot = sum(abund), .by = c(sim, region, year, species_code)) %>% 
                                 tidytable::mutate(prop = abund / tot,
                                                   sex_cat = 12) %>% 
                                 tidytable::select(sim, region, year, species_code, sex, length, prop)) %>% 
          # compute comps for sex category 4
          tidytable::bind_rows(data %>% 
                                 tidytable::filter(sex != 0) %>% 
                                 tidytable::summarise(abund = sum(abund), .by = c(sim, region, year, species_code, length)) %>% 
                                 tidytable::mutate(tot = sum(abund), .by = c(sim, region, year, species_code)) %>% 
                                 tidytable::mutate(prop = abund / tot,
                                                   sex = 4,
                                                   sex_cat = 4) %>% 
                                 tidytable::select(sim, region, year, species_code, sex, length, prop)) -> data
        # get bs stats
        summ <- get_stats(data = data,
                          grp = c('year', 'region', 'species_code', 'sex', 'length'),
                          column = prop)
      } else{ # without subregion case
        # compute comps for sex categories 0, 1, and 2
        data %>% 
          tidytable::filter(sex %in% c(0, 1, 2)) %>% 
          tidytable::mutate(tot = sum(abund), .by = c(sim, year, species_code, sex)) %>% 
          tidytable::mutate(prop = abund / tot,
                            sex_cat = sex) %>% 
          tidytable::select(sim, year, species_code, sex, length, prop) %>% 
          # compute comps for sex category 12
          tidytable::bind_rows(data %>% 
                                 tidytable::filter(sex %in% c(1, 2)) %>% 
                                 tidytable::mutate(tot = sum(abund), .by = c(sim, year, species_code)) %>% 
                                 tidytable::mutate(prop = abund / tot,
                                                   sex_cat = 12) %>% 
                                 tidytable::select(sim, year, species_code, sex, length, prop)) %>% 
          # compute comps for sex category 4
          tidytable::bind_rows(data %>% 
                                 tidytable::filter(sex != 0) %>% 
                                 tidytable::summarise(abund = sum(abund), .by = c(sim, year, species_code, length)) %>% 
                                 tidytable::mutate(tot = sum(abund), .by = c(sim, year, species_code)) %>% 
                                 tidytable::mutate(prop = abund / tot,
                                                   sex = 4,
                                                   sex_cat = 4) %>% 
                                 tidytable::select(sim, year, species_code, sex, length, prop)) -> data
        # get bs stats
        summ <- get_stats(data = data,
                          grp = c('year', 'species_code', 'sex', 'length'),
                          column = prop)
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
                            sex_cat = sex) %>% 
          tidytable::select(sim, region, year, species_code, sex, age, prop) %>% 
          # compute comps for sex category 12
          tidytable::bind_rows(data %>% 
                                 tidytable::filter(sex %in% c(1, 2)) %>% 
                                 tidytable::mutate(tot = sum(agepop), .by = c(sim, region, year, species_code)) %>% 
                                 tidytable::mutate(prop = agepop / tot,
                                                   sex_cat = 12) %>% 
                                 tidytable::select(sim, region, year, species_code, sex, age, prop)) %>% 
          # compute comps for sex category 4
          tidytable::bind_rows(data %>% 
                                 tidytable::filter(sex != 0) %>% 
                                 tidytable::summarise(agepop = sum(agepop), .by = c(sim, region, year, species_code, age)) %>% 
                                 tidytable::mutate(tot = sum(agepop), .by = c(sim, region, year, species_code)) %>% 
                                 tidytable::mutate(prop = agepop / tot,
                                                   sex = 4,
                                                   sex_cat = 4) %>% 
                                 tidytable::select(sim, region, year, species_code, sex, age, prop)) -> data
        # get bs stats
        summ <- get_stats(data = data,
                          grp = c('year', 'region', 'species_code', 'sex', 'age'),
                          column = prop)
      } else{ # without subregion case
        # compute comps for sex categories 0, 1, and 2
        data %>% 
          tidytable::filter(sex %in% c(0, 1, 2)) %>% 
          tidytable::mutate(tot = sum(agepop), .by = c(sim, year, species_code, sex)) %>% 
          tidytable::mutate(prop = agepop / tot,
                            sex_cat = sex) %>% 
          tidytable::select(sim, year, species_code, sex, age, prop) %>% 
          # compute comps for sex category 12
          tidytable::bind_rows(data %>% 
                                 tidytable::filter(sex %in% c(1, 2)) %>% 
                                 tidytable::mutate(tot = sum(agepop), .by = c(sim, year, species_code)) %>% 
                                 tidytable::mutate(prop = agepop / tot,
                                                   sex_cat = 12) %>% 
                                 tidytable::select(sim, year, species_code, sex, age, prop)) %>% 
          # compute comps for sex category 4
          tidytable::bind_rows(data %>% 
                                 tidytable::filter(sex != 0) %>% 
                                 tidytable::summarise(agepop = sum(agepop), .by = c(sim, year, species_code, age)) %>% 
                                 tidytable::mutate(tot = sum(agepop), .by = c(sim, year, species_code)) %>% 
                                 tidytable::mutate(prop = agepop / tot,
                                                   sex = 4,
                                                   sex_cat = 4) %>% 
                                 tidytable::select(sim, year, species_code, sex, age, prop)) -> data
        # get bs stats
        summ <- get_stats(data = data,
                          grp = c('year', 'species_code', 'sex', 'age'),
                          column = prop)
      }
    }
    # for caal
    if('caal' %in% colnames(data)){
      summ <- get_stats(data = data,
                        grp = c('year', 'species_code', 'sex', 'length', 'age'),
                        column = caal)
    }
    
    #for rss results
    if('rss' %in% colnames(data)){
      # with subregion case
      if('region' %in% colnames(data)){
        summ <- get_stats(data = data,
                          grp = c('year', 'region', 'species_code', 'sex', 'sex_desc'),
                          column = rss)
      } else{ # without subregion case
        summ <- get_stats(data = data,
                          grp = c('year', 'species_code', 'sex', 'sex_desc'),
                          column = rss)
      }
      # for caal
      if('length' %in% colnames(data)){
        summ <- get_stats(data = data,
                          grp = c('year', 'species_code', 'sex', 'sex_desc', 'length'),
                          column = rss)
      }
    }
  } else{
    summ <- data
  }
  
  cat(crayon::red(iter), "of", crayon::green(crayon::bold(tot)), crayon::blue(crayon::bold(toupper(reg))), "objects summarized\n")
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

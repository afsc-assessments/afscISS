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

  # pull surveyISS results ----
  ## ai results ----
  ai <- purrr::map(list.files(here::here('output', 'ai')), ~ vroom::vroom(here::here('output', 'ai', .)))
  ai <- purrr::map(1:length(ai), ~ summ_stats(ai[[.]], ., length(ai), 'ai'))
  names(ai) <- list.files(here::here('output', 'ai')) %>% 
    stringr::str_replace(., '.csv', "")
  

  ## ebs results ----
  ebs <- purrr::map(list.files(here::here('output', 'ebs')), ~ vroom::vroom(here::here('output', 'ebs', .)))
  ebs <- purrr::map(1:length(ebs), ~ summ_stats(ebs[[.]], ., length(ebs), 'ebs'))
  names(ebs) <- list.files(here::here('output', 'ebs')) %>% 
    stringr::str_replace(., '.csv', "")

  ## ebs_slope results ----
  ebs_slope <- purrr::map(list.files(here::here('output', 'ebs_slope')), ~ vroom::vroom(here::here('output', 'ebs_slope', .)))
  ebs_slope <- purrr::map(1:length(ebs_slope), ~ summ_stats(ebs_slope[[.]], ., length(ebs_slope), 'ebs_slope'))
  names(ebs_slope) <- list.files(here::here('output', 'ebs_slope')) %>% 
    stringr::str_replace(., '.csv', "")
  

  ## goa results ----
  goa <- purrr::map(list.files(here::here('output', 'goa')), ~ vroom::vroom(here::here('output', 'goa', .)))
  goa <- purrr::map(1:length(goa), ~ summ_stats(goa[[.]], ., length(goa), 'goa'))
  names(goa) <- list.files(here::here('output', 'goa')) %>% 
    stringr::str_replace(., '.csv', "")
  
  ## nebs results ----
  nebs <- purrr::map(list.files(here::here('output', 'nebs')), ~ vroom::vroom(here::here('output', 'nebs', .)))
  nebs <- purrr::map(1:length(nebs), ~ summ_stats(nebs[[.]], ., length(nebs), 'nebs'))
  names(nebs) <- list.files(here::here('output', 'nebs')) %>% 
    stringr::str_replace(., '.csv', "")

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

#' function to summarize bootstrap results
#' 
#' @description
#' Function that computes mean and percentiles for iterated (comps and rss) results from the surveyISS package.
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
#' Function that tests whether summary statistics should be performed on results, and computes statistics with get_stats function
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
        summ <- get_stats(data = data,
                          grp = c('year', 'region', 'species_code', 'sex', 'length'),
                          column = abund)
      } else{ # without subregion case
        summ <- get_stats(data = data,
                          grp = c('year', 'species_code', 'sex', 'length'),
                          column = abund)
      }
    }
    # for age comps
    if('agepop' %in% colnames(data)){
      # with subregion case
      if('region' %in% colnames(data)){
        summ <- get_stats(data = data,
                          grp = c('year', 'region', 'species_code', 'sex', 'age'),
                          column = agepop)
      } else{ # without subregion case
        summ <- get_stats(data = data,
                          grp = c('year', 'species_code', 'sex', 'age'),
                          column = agepop)
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
  
  summ
  
  print(paste(iter, "of", tot, reg, "objects summarized"))
  
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

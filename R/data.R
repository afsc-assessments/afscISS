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
pkg_dta <- function(append = TRUE) {
  
  if(isTRUE(append)){
    
    
  } else{
    # ai results ----
    ai <- purrr::map(list.files(here::here('output', 'ai')), ~ vroom::vroom(here::here('output', 'ai', .)))
    names(ai) <- list.files(here::here('output', 'ai')) %>% 
      stringr::str_replace(., '.csv', "")
    
    # ebs results ----
    ebs <- purrr::map(list.files(here::here('output', 'ebs')), ~ vroom::vroom(here::here('output', 'ebs', .)))
    names(ebs) <- list.files(here::here('output', 'ebs')) %>% 
      stringr::str_replace(., '.csv', "")
    
    # ebs_slope results ----
    ebs_slope <- purrr::map(list.files(here::here('output', 'ebs_slope')), ~ vroom::vroom(here::here('output', 'ebs_slope', .)))
    names(ebs_slope) <- list.files(here::here('output', 'ebs_slope')) %>% 
      stringr::str_replace(., '.csv', "")
    
    # goa results ----
    goa <- purrr::map(list.files(here::here('output', 'goa')), ~ vroom::vroom(here::here('output', 'goa', .)))
    names(goa) <- list.files(here::here('output', 'goa')) %>% 
      stringr::str_replace(., '.csv', "")
    
    # nebs results ----
    nebs <- purrr::map(list.files(here::here('output', 'nebs')), ~ vroom::vroom(here::here('output', 'nebs', .)))
    names(nebs) <- list.files(here::here('output', 'nebs')) %>% 
      stringr::str_replace(., '.csv', "")
    
    # write out results as package data ----
    
    data_iss <- list(ai, ebs, ebs_slope, goa, nebs)
    names(data_iss) <- c('ai', 'ebs', 'ebs_slope', 'goa', 'nebs')
    usethis::use_data(data_iss)
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

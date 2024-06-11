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
    usethis::use_data(ai)
    
    # ebs results ----
    ebs <- purrr::map(list.files(here::here('output', 'ebs')), ~ vroom::vroom(here::here('output', 'ebs', .)))
    names(ebs) <- list.files(here::here('output', 'ebs')) %>% 
      stringr::str_replace(., '.csv', "")
    usethis::use_data(ebs)
    
    # ebs_slope results ----
    ebs_slope <- purrr::map(list.files(here::here('output', 'ebs_slope')), ~ vroom::vroom(here::here('output', 'ebs_slope', .)))
    names(ebs_slope) <- list.files(here::here('output', 'ebs_slope')) %>% 
      stringr::str_replace(., '.csv', "")
    usethis::use_data(ebs_slope)
    
    # goa results ----
    goa <- purrr::map(list.files(here::here('output', 'goa')), ~ vroom::vroom(here::here('output', 'goa', .)))
    names(goa) <- list.files(here::here('output', 'goa')) %>% 
      stringr::str_replace(., '.csv', "")
    usethis::use_data(goa)
    
    # nebs results ----
    nebs <- purrr::map(list.files(here::here('output', 'nebs')), ~ vroom::vroom(here::here('output', 'nebs', .)))
    names(nebs) <- list.files(here::here('output', 'nebs')) %>% 
      stringr::str_replace(., '.csv', "")
    usethis::use_data(nebs)
  }

}

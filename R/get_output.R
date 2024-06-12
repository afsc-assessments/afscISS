#' function to get stock-specific ISS results
#' 
#' @description
#' Function that retrieves composition ISS results for AFSC stock assessments
#' 
#' @param species AFSC species code (default = 21720, pacific cod)
#' @param region survey region. options are 'ai', 'ebs', 'ebs_slope', 'goa', and 'nebs' (default = 'goa')
#' @param comp type of composition for which ISS desired. options are 'age', 'length', and 'caal'
#' @param sex_cat sex category for which composition ISS desired. options are 0, 1, 2, 12, and 4 (default = 4)
#' @param spec_case description string if getting ISS for special case. options are 'ai_subreg', 'bsre', 'dr', 'rebs', 'w_c_egoa', 'w140', 'wc_egoa' (default = NULL)
#'
#' @return a dataframe of composition ISS
#' 
#' @export
#'
get_ISS <- function(species = 21720,
                    region = 'goa',
                    comp = 'age',
                    sex_cat = 4,
                    spec_case = NULL) {
  
  # age comp iss ----
  if(comp == 'age'){
    if(is.null(spec_case)){
      tidytable::as_tidytable(data_iss[[region]]$prod_iss_ag) %>%
        tidytable::filter(species_code %in% species,
                          sex %in% sex_cat) -> res
    } else{
      if(spec_case %in% c('bsre', 'dr', 'rebs')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_iss_ag_', spec_case)]]) %>%
          tidytable::filter(sex %in% sex_cat) -> res
      } else{
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_iss_ag_', spec_case)]]) %>%
          tidytable::filter(species_code %in% species,
                            sex %in% sex_cat) -> res
      }
    }
  }
  
  # length comp iss ----
  if(comp == 'length'){
    if(is.null(spec_case)){
      tidytable::as_tidytable(data_iss[[region]]$prod_iss_ln) %>%
        tidytable::filter(species_code %in% species,
                          sex %in% sex_cat) -> res
    } else{
      if(spec_case %in% c('bsre', 'dr', 'rebs')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_iss_ln_', spec_case)]]) %>%
          tidytable::filter(sex %in% sex_cat) -> res
      } else{
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_iss_ln_', spec_case)]]) %>%
          tidytable::filter(species_code %in% species,
                            sex %in% sex_cat) -> res
      }
    }
  } 
  
  # caal iss ----
  if(comp == 'caal'){
    tidytable::as_tidytable(data_iss[[region]]$prod_iss_caal) %>%
      tidytable::filter(species_code %in% species,
                        sex %in% sex_cat) -> res
  }
  
  res
  
}


#' function to get stock-specific composition results
#' 
#' @description
#' Function that retrieves age and length pop'n numbers and conditional age-at-length for AFSC stock assessments
#' 
#' @param species AFSC species code (default = 21720, pacific cod)
#' @param region survey region. options are 'ai', 'ebs', 'ebs_slope', 'goa', and 'nebs' (default = 'goa')
#' @param comp type of composition for which ISS desired. options are 'age', 'length', and 'caal'
#' @param sex_cat sex category for which composition ISS desired. options are 0, 1, 2, 12, and 4 (default = 4)
#' @param spec_case description string if getting ISS for special case. options are 'ai_subreg', 'bsre', 'dr', 'rebs', 'w_c_egoa', 'w140', 'wc_egoa' (default = NULL)
#'
#' @return a dataframe of age-length pop'n numbers or conditional age-at-length
#' 
#' @export
#'
get_comp <- function(species = 21720,
                     region = 'goa',
                     comp = 'age',
                     sex_cat = 4,
                     spec_case = NULL) {
  
  # age pop'n ----
  if(comp == 'age'){
    if(is.null(spec_case)){
      tidytable::as_tidytable(data_iss[[region]]$prod_base_age) %>%
        tidytable::filter(species_code %in% species,
                          sex %in% sex_cat) -> res
    } else{
      if(spec_case %in% c('bsre', 'dr', 'rebs')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_base_age_', spec_case)]]) %>%
          tidytable::filter(sex %in% sex_cat) -> res
      } else{
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_base_age_', spec_case)]]) %>%
          tidytable::filter(species_code %in% species,
                            sex %in% sex_cat) -> res
      }
    }
  }
  
  # length pop'n ----
  if(comp == 'length'){
    if(is.null(spec_case)){
      tidytable::as_tidytable(data_iss[[region]]$prod_base_length) %>%
        tidytable::filter(species_code %in% species,
                          sex %in% sex_cat) -> res
    } else{
      if(spec_case %in% c('bsre', 'dr', 'rebs')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_base_length_', spec_case)]]) %>%
          tidytable::filter(sex %in% sex_cat) -> res
      } else{
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_base_length_', spec_case)]]) %>%
          tidytable::filter(species_code %in% species,
                            sex %in% sex_cat) -> res
      }
    }
  } 

  # caal ----
  if(comp == 'caal'){
    tidytable::as_tidytable(data_iss[[region]]$prod_base_caal) %>%
      tidytable::filter(species_code %in% species,
                        sex %in% sex_cat) -> res
  }
  
  res
  
}


#' function to get stock-specific bootstrap bias results
#' 
#' @description
#' Function that retrieves bootstrap bias results
#' 
#' @param species AFSC species code (default = 21720, pacific cod)
#' @param region survey region. options are 'ai', 'ebs', 'ebs_slope', 'goa', and 'nebs' (default = 'goa')
#' @param comp type of composition for which ISS desired. options are 'age', 'length', and 'caal'
#' @param sex_cat sex category for which composition ISS desired. options are 0, 1, 2, 12, and 4 (default = 4)
#' @param spec_case description string if getting ISS for special case. options are 'ai_subreg', 'bsre', 'dr', 'rebs', 'w_c_egoa', 'w140', 'wc_egoa' (default = NULL)
#'
#' @return a dataframe of bias statistics
#' 
#' @export
#'
get_bias <- function(species = 21720,
                     region = 'goa',
                     comp = 'age',
                     sex_cat = 4,
                     spec_case = NULL) {
  
  # age bias ----
  if(comp == 'age'){
    if(is.null(spec_case)){
      tidytable::as_tidytable(data_iss[[region]]$prod_bias_age) %>%
        tidytable::filter(species_code %in% species,
                          sex %in% sex_cat) -> res
    } else{
      if(spec_case %in% c('bsre', 'dr', 'rebs')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_bias_age_', spec_case)]]) %>%
          tidytable::filter(sex %in% sex_cat) -> res
      } else{
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_bias_age_', spec_case)]]) %>%
          tidytable::filter(species_code %in% species,
                            sex %in% sex_cat) -> res
      }
    }
  }
  
  # length bias ----
  if(comp == 'length'){
    if(is.null(spec_case)){
      tidytable::as_tidytable(data_iss[[region]]$prod_bias_length) %>%
        tidytable::filter(species_code %in% species,
                          sex %in% sex_cat) -> res
    } else{
      if(spec_case %in% c('bsre', 'dr', 'rebs')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_bias_length_', spec_case)]]) %>%
          tidytable::filter(sex %in% sex_cat) -> res
      } else{
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_bias_length_', spec_case)]]) %>%
          tidytable::filter(species_code %in% species,
                            sex %in% sex_cat) -> res
      }
    }
  } 
  
  # caal bias ----
  if(comp == 'caal'){
    tidytable::as_tidytable(data_iss[[region]]$prod_bias_caal) %>%
      tidytable::filter(species_code %in% species,
                        sex %in% sex_cat) -> res
  }
  
  res
  
}
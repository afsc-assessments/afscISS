#' Get stock-specific ISS results
#' 
#' @description
#' Function that retrieves composition Input Sample Size results for AFSC stock assessments
#' 
#' @param species AFSC species code (default = 21720, pacific cod)
#' @param region survey region. options are 'ai', 'ebs', 'ebs_slope', 'goa', and 'nebs' (default = 'goa')
#' @param comp type of composition for which ISS desired. options are 'age', 'length', and 'caal'
#' @param sex_cat sex category for which composition ISS desired. options are 0 (sexes combined pre-expansion), 1 (males), 2 (females), 12 (males-female comp that sums to 1), and 4 (sexes combined post-expansion) (default = 4)
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
  data_iss = afscISS::data_iss
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
      } 
      if(spec_case %in% c('ai_subreg', 'w_c_egoa', 'wc_egoa', 'w140')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_iss_ag_', spec_case)]]) %>%
          tidytable::filter(species_code %in% species,
                            sex %in% sex_cat) -> res
      }
      if(spec_case %in% c('bin')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_', spec_case, '_iss_ag')]]) %>%
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
      } 
      if(spec_case %in% c('ai_subreg', 'w_c_egoa', 'wc_egoa', 'w140')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_iss_ln_', spec_case)]]) %>%
          tidytable::filter(species_code %in% species,
                            sex %in% sex_cat) -> res
      }
      if(spec_case %in% c('bin')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_', spec_case, '_iss_ln')]]) %>%
          tidytable::filter(species_code %in% species,
                            sex %in% sex_cat) -> res
      }
    }
  } 
  
  # caal iss ----
  if(comp == 'caal'){
    if(is.null(spec_case)){
      tidytable::as_tidytable(data_iss[[region]]$prod_iss_caal) %>%
        tidytable::filter(species_code %in% species) -> res1
      if(sex_cat %in% c(0, 4)){
        res1 %>% 
          tidytable::filter(sex == 0) -> res
      } else{
        res1 %>% 
          tidytable::filter(sex == sex_cat) -> res
      }
    } else{
      if(spec_case %in% c('bin')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_', spec_case, '_iss_caal')]]) %>%
          tidytable::filter(species_code %in% species) -> res1
        if(sex_cat %in% c(0, 4)){
          res1 %>% 
            tidytable::filter(sex == 0) -> res
        } else{
          res1 %>% 
            tidytable::filter(sex == sex_cat) -> res
        }
      }
    }
  }
  
  res
  
}


#' Get stock-specific numbers at age/length results
#' 
#' @description
#' Function that retrieves age and length pop'n numbers for AFSC stock assessments
#' 
#' @param species AFSC species code (default = 21720, pacific cod)
#' @param region survey region. options are 'ai', 'ebs', 'ebs_slope', 'goa', and 'nebs' (default = 'goa')
#' @param comp type of pop'n numbers desired, options are 'age' and 'length'
#' @param sex_cat sex category for which pop'n numbers desired. options are 0 (sexes combined pre-expansion), 1 (males), 2 (females), 3 (unsexed), and 4 (sexes combined post-expansion) (default = 4)
#' @param spec_case description string if getting pop'n numbers for special case. options are 'ai_subreg', 'bsre', 'dr', 'rebs', 'w_c_egoa', 'w140', 'wc_egoa' (default = NULL)
#'
#' @return a dataframe of age or length pop'n numbers (note that age data also include mean and sd in length-at-age)
#' 
#' @export
#'
get_popn <- function(species = 21720,
                     region = 'goa',
                     comp = 'age',
                     sex_cat = 4,
                     spec_case = NULL) {
  data_iss = afscISS::data_iss
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
      } 
      if(spec_case %in% c('ai_subreg', 'w_c_egoa', 'wc_egoa', 'w140')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_base_age_', spec_case)]]) %>%
          tidytable::filter(species_code %in% species,
                            sex %in% sex_cat) -> res
      }
      if(spec_case %in% c('bin')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_', spec_case, '_base_age')]]) %>%
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
      } 
      if(spec_case %in% c('ai_subreg', 'w_c_egoa', 'wc_egoa', 'w140')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_base_length_', spec_case)]]) %>%
          tidytable::filter(species_code %in% species,
                            sex %in% sex_cat) -> res
      }
      if(spec_case %in% c('bin')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_', spec_case, '_base_length')]]) %>%
          tidytable::filter(species_code %in% species,
                            sex %in% sex_cat) -> res
      }
    }
  } 
  
  res
  
}

#' Get stock-specific composition results
#' 
#' @description
#' Function that retrieves age, length, and conditional age-at-length composition for AFSC stock assessments
#' 
#' @param species AFSC species code (default = 21720, pacific cod)
#' @param region survey region. options are 'ai', 'ebs', 'ebs_slope', 'goa', and 'nebs' (default = 'goa')
#' @param comp type of composition desired, options are 'age', 'length', and 'caal'
#' @param sex_cat sex category for which composition is desired. options are 0 (sexes combined pre-expansion), 1 (males), 2 (females), 12 (males-female comp that sums to 1), and 4 (sexes combined post-expansion). Note that for comp = 'caal' the only sex categories available are 0 (sexes combined), 1 (males), and 2 (females) (default = 4)
#' @param spec_case description string if getting composition for special case. options are 'ai_subreg', 'bsre', 'dr', 'rebs', 'w_c_egoa', 'w140', 'wc_egoa' (default = NULL)
#'
#' @return a dataframe of age, length, or conditional age-at-length composition
#' 
#' @export
#'
get_comp <- function(species = 21720,
                     region = 'goa',
                     comp = 'age',
                     sex_cat = 4,
                     spec_case = NULL) {
  data_iss = afscISS::data_iss
  # age comp ----
  if(comp == 'age'){
    if(is.null(spec_case)){
      tidytable::as_tidytable(data_iss[[region]]$prod_comp_age) %>%
        tidytable::filter(species_code %in% species,
                          sex_c %in% sex_cat) -> res
    } else{
      if(spec_case %in% c('bsre', 'dr', 'rebs')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_comp_age_', spec_case)]]) %>%
          tidytable::filter(sex_c %in% sex_cat) -> res
      } 
      if(spec_case %in% c('ai_subreg', 'w_c_egoa', 'wc_egoa', 'w140')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_comp_age_', spec_case)]]) %>%
          tidytable::filter(species_code %in% species,
                            sex_c %in% sex_cat) -> res
      }
      if(spec_case %in% c('bin')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_', spec_case, '_comp_age')]]) %>%
          tidytable::filter(species_code %in% species,
                            sex_c %in% sex_cat) -> res
      }
    }
  }
  
  # length comp ----
  if(comp == 'length'){
    if(is.null(spec_case)){
      tidytable::as_tidytable(data_iss[[region]]$prod_comp_length) %>%
        tidytable::filter(species_code %in% species,
                          sex_c %in% sex_cat) -> res
    } else{
      if(spec_case %in% c('bsre', 'dr', 'rebs')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_comp_length_', spec_case)]]) %>%
          tidytable::filter(sex_c %in% sex_cat) -> res
      }
      if(spec_case %in% c('ai_subreg', 'w_c_egoa', 'wc_egoa', 'w140')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_comp_length_', spec_case)]]) %>%
          tidytable::filter(species_code %in% species,
                            sex_c %in% sex_cat) -> res
      }
      if(spec_case %in% c('bin')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_', spec_case, '_comp_length')]]) %>%
          tidytable::filter(species_code %in% species,
                            sex_c %in% sex_cat) -> res
      }
    }
  } 
  
  # caal comp ----
  if(comp == 'caal'){
    if(is.null(spec_case)){
      tidytable::as_tidytable(data_iss[[region]]$prod_comp_caal) %>%
        tidytable::filter(species_code %in% species) -> res1
      if(sex_cat %in% c(0, 4)){
        res1 %>% 
          tidytable::filter(sex == 0) -> res
      } else{
        res1 %>% 
          tidytable::filter(sex == sex_cat) -> res
      }
    }else{
      if(spec_case %in% c('bin')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_', spec_case, '_comp_caal')]]) %>%
          tidytable::filter(species_code %in% species) -> res1
        if(sex_cat %in% c(0, 4)){
          res1 %>% 
            tidytable::filter(sex == 0) -> res
        } else{
          res1 %>% 
            tidytable::filter(sex == sex_cat) -> res
        }
      }
    }
  }
  
  res
  
}

#' Get stock-specific bootstrap bias results
#' 
#' @description
#' Function that retrieves bootstrap bias results
#' 
#' @param species AFSC species code (default = 21720, pacific cod)
#' @param region survey region. options are 'ai', 'ebs', 'ebs_slope', 'goa', and 'nebs' (default = 'goa')
#' @param comp type of composition for which ISS desired. options are 'age', 'length', and 'caal'
#' @param sex_cat sex category for which bootstrap bias desired, options are 0 (sexes combined pre-expansion), 1 (males), 2 (females), 12 (males-female comp that sums to 1), and 4 (sexes combined post-expansion) (default = 4)
#' @param spec_case description string if getting bootstrap bias for special case. options are 'ai_subreg', 'bsre', 'dr', 'rebs', 'w_c_egoa', 'w140', 'wc_egoa' (default = NULL)
#'
#' @return a dataframe of bootstrap bias statistics
#' 
#' @export
#'
get_bias <- function(species = 21720,
                     region = 'goa',
                     comp = 'age',
                     sex_cat = 4,
                     spec_case = NULL) {
  data_iss = afscISS::data_iss
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
      } 
      if(spec_case %in% c('ai_subreg', 'w_c_egoa', 'wc_egoa', 'w140')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_bias_age_', spec_case)]]) %>%
          tidytable::filter(species_code %in% species,
                            sex %in% sex_cat) -> res
      }
      if(spec_case %in% c('bin')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_', spec_case, '_bias_age')]]) %>%
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
      } 
      if(spec_case %in% c('ai_subreg', 'w_c_egoa', 'wc_egoa', 'w140')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_bias_length_', spec_case)]]) %>%
          tidytable::filter(species_code %in% species,
                            sex %in% sex_cat) -> res
      }
      if(spec_case %in% c('bin')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_', spec_case, '_bias_length')]]) %>%
          tidytable::filter(species_code %in% species,
                            sex %in% sex_cat) -> res
      }
    }
  } 
  
  # caal bias ----
  if(comp == 'caal'){
    if(is.null(spec_case)){
      tidytable::as_tidytable(data_iss[[region]]$prod_bias_caal) %>%
        tidytable::filter(species_code %in% species) -> res1
      if(sex_cat %in% c(0, 4)){
        res1 %>% 
          tidytable::filter(sex == 0) -> res
      } else{
        res1 %>% 
          tidytable::filter(sex == sex_cat) -> res
      }
    } else{
      if(spec_case %in% c('bin')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_', spec_case, '_bias_caal')]]) %>%
          tidytable::filter(species_code %in% species) -> res1
        if(sex_cat %in% c(0, 4)){
          res1 %>% 
            tidytable::filter(sex == 0) -> res
        } else{
          res1 %>% 
            tidytable::filter(sex == sex_cat) -> res
        }
      }
    }
  }
  
  res
  
}

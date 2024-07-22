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
                          sex %in% sex_cat) %>% 
        tidytable::mutate(iss = round(iss, digits = 0)) -> res
    } else{
      if(spec_case %in% c('bsre', 'dr', 'rebs')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_iss_ag_', spec_case)]]) %>%
          tidytable::filter(sex %in% sex_cat) %>% 
          tidytable::mutate(iss = round(iss, digits = 0)) -> res
      } else{
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_iss_ag_', spec_case)]]) %>%
          tidytable::filter(species_code %in% species,
                            sex %in% sex_cat) %>% 
          tidytable::mutate(iss = round(iss, digits = 0)) -> res
      }
    }
  }
  
  # length comp iss ----
  if(comp == 'length'){
    if(is.null(spec_case)){
      tidytable::as_tidytable(data_iss[[region]]$prod_iss_ln) %>%
        tidytable::filter(species_code %in% species,
                          sex %in% sex_cat) %>% 
        tidytable::mutate(iss = round(iss, digits = 0)) -> res
    } else{
      if(spec_case %in% c('bsre', 'dr', 'rebs')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_iss_ln_', spec_case)]]) %>%
          tidytable::filter(sex %in% sex_cat) %>% 
          tidytable::mutate(iss = round(iss, digits = 0)) -> res
      } else{
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_iss_ln_', spec_case)]]) %>%
          tidytable::filter(species_code %in% species,
                            sex %in% sex_cat) %>% 
          tidytable::mutate(iss = round(iss, digits = 0)) -> res
      }
    }
  } 
  
  # caal iss ----
  if(comp == 'caal'){
    tidytable::as_tidytable(data_iss[[region]]$prod_iss_caal) %>%
      tidytable::filter(species_code %in% species,
                        sex %in% sex_cat) %>% 
      tidytable::mutate(iss = round(iss, digits = 2)) -> res
  }
  
  res
  
}


#' Get stock-specific composition results
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
  # age pop'n ----
  if(comp == 'age'){
    if(is.null(spec_case)){
      tidytable::as_tidytable(data_iss[[region]]$prod_base_age) %>%
        # for sex categories 0, 1, 2, and 4
        tidytable::filter(sex %in% c(0, 1, 2, 4)) %>% 
        tidytable::mutate(tot = sum(agepop), .by = c(year, species_code, sex)) %>% 
        tidytable::mutate(prop = agepop / tot,
                          sex_c = sex) %>% 
        tidytable::select(year, species_code, sex, sex_c, age, prop) %>% 
        # for sex category 12
        tidytable::bind_rows(tidytable::as_tidytable(data_iss[[region]]$prod_base_age) %>%
                               tidytable::filter(sex %in% c(1, 2)) %>% 
                               tidytable::mutate(tot = sum(agepop), .by = c(year, species_code)) %>% 
                               tidytable::mutate(prop = agepop / tot,
                                                 sex_c = 12) %>% 
                               tidytable::select(year, species_code, sex, sex_c, age, prop)) %>% 
        tidytable::filter(species_code %in% species,
                          sex_c %in% sex_cat) %>% 
        tidytable::select(-sex_c) -> res
    } else{
      if(spec_case %in% c('bsre', 'dr', 'rebs')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_base_age_', spec_case)]]) %>%
          # for sex categories 0, 1, 2, and 4
          tidytable::filter(sex %in% c(0, 1, 2, 4)) %>% 
          tidytable::mutate(tot = sum(agepop), .by = c(year, species_code, sex)) %>% 
          tidytable::mutate(prop = agepop / tot,
                            sex_c = sex) %>% 
          tidytable::select(year, species_code, sex, sex_c, age, prop) %>% 
          # for sex category 12
          tidytable::bind_rows(tidytable::as_tidytable(data_iss[[region]][[paste0('prod_base_age_', spec_case)]]) %>%
                                 tidytable::filter(sex %in% c(1, 2)) %>% 
                                 tidytable::mutate(tot = sum(agepop), .by = c(year, species_code)) %>% 
                                 tidytable::mutate(prop = agepop / tot,
                                                   sex_c = 12) %>% 
                                 tidytable::select(year, species_code, sex, sex_c, age, prop)) %>% 
          tidytable::filter(sex_c %in% sex_cat) %>% 
          tidytable::select(-sex_c) -> res
      } else{
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_base_age_', spec_case)]]) %>%
          # for sex categories 0, 1, 2, and 4
          tidytable::filter(sex %in% c(0, 1, 2, 4)) %>% 
          tidytable::mutate(tot = sum(agepop), .by = c(year, region, species_code, sex)) %>% 
          tidytable::mutate(prop = agepop / tot,
                            sex_c = sex) %>% 
          tidytable::select(year, region, species_code, sex, sex_c, age, prop) %>% 
          # for sex category 12
          tidytable::bind_rows(tidytable::as_tidytable(data_iss[[region]][[paste0('prod_base_age_', spec_case)]]) %>%
                                 tidytable::filter(sex %in% c(1, 2)) %>% 
                                 tidytable::mutate(tot = sum(agepop), .by = c(year, region, species_code)) %>% 
                                 tidytable::mutate(prop = agepop / tot,
                                                   sex_c = 12) %>% 
                                 tidytable::select(year, region, species_code, sex, sex_c, age, prop)) %>% 
          tidytable::filter(species_code %in% species,
                            sex_c %in% sex_cat) %>% 
          tidytable::select(-sex_c) -> res
      }
    }
  }
  
  # length pop'n ----
  if(comp == 'length'){
    if(is.null(spec_case)){
      tidytable::as_tidytable(data_iss[[region]]$prod_base_length) %>%
        # for sex categories 0, 1, 2, and 4
        tidytable::filter(sex %in% c(0, 1, 2, 4)) %>% 
        tidytable::mutate(tot = sum(abund), .by = c(year, species_code, sex)) %>% 
        tidytable::mutate(prop = abund / tot,
                          sex_c = sex) %>% 
        tidytable::select(year, species_code, sex, sex_c, length, prop) %>% 
        # for sex category 12
        tidytable::bind_rows(tidytable::as_tidytable(data_iss[[region]]$prod_base_length) %>%
                               tidytable::filter(sex %in% c(1, 2)) %>% 
                               tidytable::mutate(tot = sum(abund), .by = c(year, species_code)) %>% 
                               tidytable::mutate(prop = abund / tot,
                                                 sex_c = 12) %>% 
                               tidytable::select(year, species_code, sex, sex_c, length, prop)) %>% 
        tidytable::filter(species_code %in% species,
                          sex_c %in% sex_cat) %>% 
        tidytable::select(-sex_c) -> res
      
    } else{
      if(spec_case %in% c('bsre', 'dr', 'rebs')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_base_length_', spec_case)]]) %>%
          # for sex categories 0, 1, 2, and 4
          tidytable::filter(sex %in% c(0, 1, 2, 4)) %>% 
          tidytable::mutate(tot = sum(abund), .by = c(year, species_code, sex)) %>% 
          tidytable::mutate(prop = abund / tot,
                            sex_c = sex) %>% 
          tidytable::select(year, species_code, sex, sex_c, length, prop) %>% 
          # for sex category 12
          tidytable::bind_rows(tidytable::as_tidytable(data_iss[[region]][[paste0('prod_base_length_', spec_case)]]) %>%
                                 tidytable::filter(sex %in% c(1, 2)) %>% 
                                 tidytable::mutate(tot = sum(abund), .by = c(year, species_code)) %>% 
                                 tidytable::mutate(prop = abund / tot,
                                                   sex_c = 12) %>% 
                                 tidytable::select(year, species_code, sex, sex_c, length, prop)) %>% 
          tidytable::filter(sex_c %in% sex_cat) %>% 
          tidytable::select(-sex_c) -> res
        
        
      } else{
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_base_length_', spec_case)]]) %>%
          # for sex categories 0, 1, 2, and 4
          tidytable::filter(sex %in% c(0, 1, 2, 4)) %>% 
          tidytable::mutate(tot = sum(abund), .by = c(year, region, species_code, sex)) %>% 
          tidytable::mutate(prop = abund / tot,
                            sex_c = sex) %>% 
          tidytable::select(year, region, species_code, sex, sex_c, length, prop) %>% 
          # for sex category 12
          tidytable::bind_rows(tidytable::as_tidytable(data_iss[[region]][[paste0('prod_base_length_', spec_case)]]) %>%
                                 tidytable::filter(sex %in% c(1, 2)) %>% 
                                 tidytable::mutate(tot = sum(abund), .by = c(year, region, species_code)) %>% 
                                 tidytable::mutate(prop = abund / tot,
                                                   sex_c = 12) %>% 
                                 tidytable::select(year, region, species_code, sex, sex_c, length, prop)) %>% 
          tidytable::filter(species_code %in% species,
                            sex_c %in% sex_cat) %>% 
          tidytable::select(-sex_c) -> res
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

#' Get stock-specific RSS results
#' 
#' @description
#' Function that retrieves composition replicates of Realized Sample Size (RSS) results for AFSC stock assessments
#' 
#' @param species AFSC species code (default = 21720, pacific cod)
#' @param region survey region. options are 'ai', 'ebs', 'ebs_slope', 'goa', and 'nebs' (default = 'goa')
#' @param comp type of composition for which RSS desired, options are 'age', 'length', and 'caal'
#' @param sex_cat sex category for which RSS desired, options are 0 (sexes combined pre-expansion), 1 (males), 2 (females), 12 (males-female comp that sums to 1), and 4 (sexes combined post-expansion) (default = 4)
#' @param spec_case description string if getting RSS for special case, options are 'ai_subreg', 'bsre', 'dr', 'rebs', 'w_c_egoa', 'w140', 'wc_egoa' (default = NULL)
#'
#' @return a dataframe of bootstrap statistics for RSS
#' 
#' @export
#'
get_RSS <- function(species = 21720,
                    region = 'goa',
                    comp = 'age',
                    sex_cat = 4,
                    spec_case = NULL) {
  data_iss = afscISS::data_iss
  # age comp iss ----
  if(comp == 'age'){
    if(is.null(spec_case)){
      tidytable::as_tidytable(data_iss[[region]]$prod_iter_rss_ag) %>%
        tidytable::filter(species_code %in% species,
                          sex %in% sex_cat) -> res
    } else{
      if(spec_case %in% c('bsre', 'dr', 'rebs')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_iter_rss_ag_', spec_case)]]) %>%
          tidytable::filter(sex %in% sex_cat) -> res
      } else{
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_iter_rss_ag_', spec_case)]]) %>%
          tidytable::filter(species_code %in% species,
                            sex %in% sex_cat) -> res
      }
    }
  }
  
  # length comp iss ----
  if(comp == 'length'){
    if(is.null(spec_case)){
      tidytable::as_tidytable(data_iss[[region]]$prod_iter_rss_ln) %>%
        tidytable::filter(species_code %in% species,
                          sex %in% sex_cat) -> res
    } else{
      if(spec_case %in% c('bsre', 'dr', 'rebs')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_iter_rss_ln_', spec_case)]]) %>%
          tidytable::filter(sex %in% sex_cat) -> res
      } else{
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_iter_rss_ln_', spec_case)]]) %>%
          tidytable::filter(species_code %in% species,
                            sex %in% sex_cat) -> res
      }
    }
  } 
  
  # caal iss ----
  if(comp == 'caal'){
    tidytable::as_tidytable(data_iss[[region]]$prod_iter_rss_caal) %>%
      tidytable::filter(species_code %in% species,
                        sex %in% sex_cat) -> res
  }
  
  res
  
}

#' Get stock-specific bootstrap composition results
#' 
#' @description
#' Function that retrieves bootstrap statistics for resampled composition data for AFSC stock assessments
#' 
#' @param species AFSC species code (default = 21720, pacific cod)
#' @param region survey region. options are 'ai', 'ebs', 'ebs_slope', 'goa', and 'nebs' (default = 'goa')
#' @param comp type of composition desired, options are 'age', 'length', and 'caal'
#' @param sex_cat sex category for which composition desired, options are 0 (sexes combined pre-expansion), 1 (males), 2 (females), 12 (males-female comp that sums to 1), and 4 (sexes combined post-expansion) (default = 4)
#' @param spec_case description string if getting composition for special case, options are 'ai_subreg', 'bsre', 'dr', 'rebs', 'w_c_egoa', 'w140', 'wc_egoa' (default = NULL)
#'
#' @return a dataframe of bootstrap statistics for replicated composition data
#' 
#' @export
#'
get_bs_comp <- function(species = 21720,
                         region = 'goa',
                         comp = 'age',
                         sex_cat = 4,
                         spec_case = NULL) {
  data_iss = afscISS::data_iss
  # age pop'n ----
  if(comp == 'age'){
    if(is.null(spec_case)){
      tidytable::as_tidytable(data_iss[[region]]$prod_resampled_age) %>%
        tidytable::filter(species_code %in% species,
                          sex_c %in% sex_cat) %>% 
        tidytable::select(-sex_c) -> res
    } else{
      if(spec_case %in% c('bsre', 'dr', 'rebs')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_resampled_age_', spec_case)]]) %>%
          tidytable::filter(sex_c %in% sex_cat) %>% 
          tidytable::select(-sex_c) -> res
      } else{
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_resampled_age_', spec_case)]]) %>%
          tidytable::filter(species_code %in% species,
                            sex_c %in% sex_cat) %>% 
          tidytable::select(-sex_c) -> res
      }
    }
  }
  
  # length pop'n ----
  if(comp == 'length'){
    if(is.null(spec_case)){
      tidytable::as_tidytable(data_iss[[region]]$prod_resampled_length) %>%
        tidytable::filter(species_code %in% species,
                          sex_c %in% sex_cat) %>% 
        tidytable::select(-sex_c) -> res
    } else{
      if(spec_case %in% c('bsre', 'dr', 'rebs')){
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_resampled_length_', spec_case)]]) %>%
          tidytable::filter(sex_c %in% sex_cat) %>% 
          tidytable::select(-sex_c) -> res
      } else{
        tidytable::as_tidytable(data_iss[[region]][[paste0('prod_resampled_length_', spec_case)]]) %>%
          tidytable::filter(species_code %in% species,
                            sex_c %in% sex_cat) %>% 
          tidytable::select(-sex_c) -> res
      }
    }
  } 
  
  # caal ----
  if(comp == 'caal'){
    tidytable::as_tidytable(data_iss[[region]]$prod_resampled_caal) %>%
      tidytable::filter(species_code %in% species,
                        sex %in% sex_cat) -> res
  }
  
  res
  
}

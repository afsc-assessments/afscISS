#' function to get stock-specific ISS results
#' 
#' @description
#' Function that retrieves composition ISS results for AFSC stock assessents
#' 
#' @param species AFSC species code (default = 21720, pacific cod)
#' @param region survey region. options are 'ai', 'ebs', 'ebs_slope', 'goa', and 'nebs' (default = 'goa')
#' @param type type of composition for which ISS desired. options are 'age', 'length', and 'caal'
#' @param sex_cat sex category for which composition ISS desired. options are 0, 1, 2, 12, and 4 (default = 4)
#' @param spec_case description string if getting ISS for special case. options are 'ai_subreg', 'bsre', 'dr', 'rebs', 'w_c_egoa', 'w140', 'wc_egoa' (default = NULL)
#'
#' @return a dataframe of composition ISS
#' 
#' @export
#'
get_ISS <- function(species = 21720,
                    region = 'goa',
                    type = 'age',
                    sex_cat = 4,
                    spec_case = NULL) {
  
  # age comp iss ----
  if(type == 'age'){
    if(is.null(spec_case)){
      vroom::vroom(here::here('output', region, 'prod_iss_ag.csv')) %>% 
        tidytable::filter(species_code %in% species,
                          sex %in% sex_cat)
    } else{
      if(spec_case %in% c('bsre', 'dr', 'rebs')){
        vroom::vroom(here::here('output', region, paste0('prod_iss_ag_', spec_case, '.csv'))) %>% 
          tidytable::filter(sex %in% sex_cat)
      } else{
        vroom::vroom(here::here('output', region, paste0('prod_iss_ag_', spec_case, '.csv'))) %>% 
          tidytable::filter(species_code %in% species,
                            sex %in% sex_cat)
      }
    }
  }
  
  # length comp iss ----
  if(type == 'length'){
    if(is.null(spec_case)){
      vroom::vroom(here::here('output', region, 'prod_iss_ln.csv')) %>% 
        tidytable::filter(species_code %in% species,
                          sex %in% sex_cat)
    } else{
      if(spec_case %in% c('bsre', 'dr', 'rebs')){
        vroom::vroom(here::here('output', region, paste0('prod_iss_ln_', spec_case, '.csv'))) %>% 
          tidytable::filter(sex %in% sex_cat)
      } else{
        vroom::vroom(here::here('output', region, paste0('prod_iss_ln_', spec_case, '.csv'))) %>% 
          tidytable::filter(species_code %in% species,
                            sex %in% sex_cat)
      }
    }
  }
  
  # caal iss ----
  if(type == 'caal'){
    vroom::vroom(here::here('output', region, 'prod_iss_caal.csv')) %>% 
      tidytable::filter(species_code %in% species,
                        sex %in% sex_cat)
  }

}



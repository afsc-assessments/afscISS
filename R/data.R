#' Developer function to get surveyISS results transferred to data package
#' 
#' @description
#' Function that creates package data for afscISS (NOTE: this is not a user fcn, this is a developer/maintainer fcn)
#' 
#' @param region region for surveyISS output to be gathered (default = standard afsc survey regions)
#'
#' @return .rda files within /data folder
#' 
#' @keywords internal
#'
pkg_data <- function(region = c('ai', 'ebs', 'ebs_slope', 'goa', 'nebs')) {

  # pull surveyISS results ----
  data_iss <- purrr::map(1:length(region), ~ get_surveyISS(region[.]))
  names(data_iss) <- region
  
  # write out results as package data ----
  usethis::use_data(data_iss, overwrite = TRUE)

}

#' Developer function to pull surveyISS results
#' 
#' @description
#' Function that retrieves results from the surveyISS package (NOTE: this is not a user fcn, this is a developer/maintainer fcn)
#' 
#' @param region region for surveyISS output to be gathered
#' 
#' @return dataframe with summary results from surveyISS
#' 
#' @keywords internal
#'
get_surveyISS <- function(region){

  ### comp results ----
  base <- purrr::map(list.files(here::here('output', region), pattern = "prod_base"), ~ vroom::vroom(here::here('output', region, .)))
  names(base) <- list.files(here::here('output', region), pattern = "prod_base") %>% 
    stringr::str_replace(., '.csv', "")
  resamp <- purrr::map(list.files(here::here('output', region), pattern = "prod_resampled"), ~ vroom::vroom(here::here('output', region, .)))
  comps <- purrr::map(1:length(base), ~ get_comps(base[[.]], resamp[[.]], ., length(base), region))
  names(comps) <- list.files(here::here('output', region), pattern = "prod_base") %>% 
    stringr::str_replace(., '.csv', "") %>% 
    stringr::str_replace(., 'base', "comp")
  
  ### bias results ----
  bias <- purrr::map(list.files(here::here('output', region), pattern = "prod_bias"), ~ vroom::vroom(here::here('output', region, .)))
  names(bias) <- list.files(here::here('output', region), pattern = "prod_bias") %>% 
    stringr::str_replace(., '.csv', "")
  
  ### iss results ----
  iss1 <- purrr::map(list.files(here::here('output', region), pattern = "prod_iss"), ~ vroom::vroom(here::here('output', region, .)))
  rss <- purrr::map(list.files(here::here('output', region), pattern = "prod_iter_rss"), ~ vroom::vroom(here::here('output', region, .)))
  iss <- purrr::map(1:length(iss1), ~ iss_stats(iss1[[.]], rss[[.]], ., length(iss1), region))
  names(iss) <- list.files(here::here('output', region), pattern = "prod_iss") %>% 
    stringr::str_replace(., '.csv', "")

  c(base, comps, bias, iss)

}

#' Developer function to summarize bootstrap results for composition data
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
#' @keywords internal
#'
get_comp_stats <- function(data, grp, column, dgts){
  data  %>% 
    tidytable::filter(!is.infinite({{column}})) %>% 
    tidytable::summarise(bs_mean = round(mean({{column}}, na.rm = TRUE), digits = dgts),
                         q2_5th = round(quantile({{column}}, 0.025, na.rm = TRUE), digits = dgts),
                         q97_5th = round(quantile({{column}}, 0.975, na.rm = TRUE), digits = dgts),
                         .by = grp)
}

#' Developer function to get composition data
#' 
#' @description
#' Function that computes age/length/conditional age-at-length composition with bootstrap 95% simulation intervals (NOTE: this is not a user fcn, this is a developer/maintainer fcn)
#' 
#' @param base_data base dataframe without resampling
#' @param res_data resampled dataframe
#' @param iter which dataframe is being summarized (for print message)
#' @param tot total number of dataframes being summarized (for print message)
#' @param reg survey region in which dataframe is being summarized (for print message)
#'
#' @return dataframe with summary bootstrap statistics
#' 
#' @keywords internal
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
        # replace lci < 0 with 0 and uci > 1 with 1
        tidytable::mutate(q2_5th = tidytable::case_when(q2_5th < 0 ~ 0,
                                                        .default = q2_5th),
                          q97_5th = tidytable::case_when(q97_5th > 1 ~ 1,
                                                        .default = q97_5th)) %>% 
        tidytable::select(-bs_mean) -> comps
      
    }

  cat(crayon::yellow(iter), "of", crayon::green(tot), crayon::blue$bold$underline(toupper(reg)), "comps objects summarized", crayon::green$bold("\u2713"), "\n")
  comps
  
}

#' Developer function to get input sample size stats
#' 
#' @description
#' Function that computes the SD for input sample size (NOTE: this is not a user fcn, this is a developer/maintainer fcn)
#' 
#' @param iss_data dataframe of iss values
#' @param rss_data dataframe of rss values
#' @param iter which dataframe is being summarized (for print message)
#' @param tot total number of dataframes being summarized (for print message)
#' @param reg survey region in which dataframe is being summarized (for print message)
#'
#' @return dataframe with summary bootstrap statistics
#' 
#' @keywords internal
#'
iss_stats <- function(iss_data, rss_data, iter, tot, reg){
  
  # with subregion case
  if('region' %in% colnames(iss_data)){
    iss_data %>% 
      tidytable::left_join(rss_data %>% 
                             tidytable::summarise(v = var(1 / rss),
                                                  n = length(rss),
                                                  m = 1 / length(rss) * sum(1 / rss),
                                                  .by = c(year, region, species_code, sex, sex_desc)) %>% 
                             tidytable::mutate(sd_iss = sqrt(1 / n * v / m ^ 4)) %>% 
                             tidytable::select(year, region, species_code, sex, sex_desc, sd_iss)) %>% 
      tidytable::mutate(iss = round(iss, digits = 0),
                        sd_iss = round(sd_iss, digits = 2)) %>% 
      tidytable::select(year, region, species_code, sex, sex_desc, iss, sd_iss, nss, nhls) -> iss
  } else{ # without subregion case
    iss_data %>% 
      tidytable::left_join(rss_data %>% 
                             tidytable::summarise(v = var(1 / rss),
                                                  n = length(rss),
                                                  m = 1 / length(rss) * sum(1 / rss),
                                                  .by = c(year, species_code, sex, sex_desc)) %>% 
                             tidytable::mutate(sd_iss = sqrt(1 / n * v / m ^ 4)) %>% 
                             tidytable::select(year, species_code, sex, sex_desc, sd_iss)) %>% 
      tidytable::mutate(iss = round(iss, digits = 0),
                        sd_iss = round(sd_iss, digits = 2)) %>% 
      tidytable::select(year, species_code, sex, sex_desc, iss, sd_iss, nss, nhls) -> iss
  }
  # for caal
  if('length' %in% colnames(iss_data)){
    iss_data %>% 
      tidytable::left_join(rss_data %>% 
                             tidytable::summarise(v = var(1 / rss),
                                                  n = length(rss),
                                                  m = 1 / length(rss) * sum(1 / rss),
                                                  .by = c(year, species_code, sex, sex_desc, length)) %>% 
                             tidytable::mutate(sd_iss = sqrt(1 / n * v / m ^ 4)) %>% 
                             tidytable::select(year, species_code, sex, sex_desc, length, sd_iss)) %>% 
      tidytable::mutate(iss = round(iss, digits = 2),
                        sd_iss = round(sd_iss, digits = 3)) %>% 
      tidytable::select(year, species_code, sex, sex_desc, length, iss, sd_iss, nss, nhls) -> iss
    
  }
  
  cat(crayon::yellow(iter), "of", crayon::green(tot), crayon::blue$bold$underline(toupper(reg)), "iss objects summarized", crayon::green$bold("\u2713"), "\n")
  iss
  
}

#' Results from surveyISS package
#'
#' A list containing the various output dataframes from the surveyISS package
#'
#' @format A list
#' \describe{
#' A list of ISS, age/length pop'n numbers, age/length/conditional age-at-length composition, bootstrap bias, and mean length-at-age
#' }
"data_iss"

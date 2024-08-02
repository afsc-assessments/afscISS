#' Plot stock-specific ISS results
#' 
#' @description
#' Function that plots composition Input Sample Size results for AFSC stock assessments
#' 
#' @param species AFSC species code 
#' @param region survey region. options are 'ai', 'ebs', 'ebs_slope', 'goa', and 'nebs' (default = 'goa')
#' @param comp type of composition for which ISS desired. options are 'age', 'length', and 'caal'
#' @param sex_cat sex category for which composition ISS desired. options are 0 (sexes combined pre-expansion), 1 (males), 2 (females), 12 (males-female comp that sums to 1), and 4 (sexes combined post-expansion) (default = 4)
#' @param spec_case description string if getting ISS for special case. options are 'ai_subreg', 'bsre', 'dr', 'rebs', 'w_c_egoa', 'w140', 'wc_egoa' (default = NULL)
#'
#' @return a dataframe of composition ISS
#' 
#' @export
#'
plot_ISS <- function(species, region = 'goa', comp = 'age', sex_cat = 4, spec_case = NULL) {
  
  get_ISS(species = species,
          region = region,
          comp = comp,
          sex_cat = sex_cat,
          spec_case = spec_case) -> dat
  
  if(comp == 'length') {
    id = 'Length ISS'
  } else {
    id = 'Age ISS'
  }
  
  dat %>% 
    tidytable::mutate(lci = iss - 1.96 * sd_iss,
                      uci = iss + 1.96 * sd_iss) %>% 
    tidytable::mutate(lci = tidytable::case_when(lci < 0 ~ 0,
                                                 .default = lci)) %>% 
    dplyr::mutate(Year = factor(year)) %>% 
    ggplot2::ggplot(ggplot2::aes(Year)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lci,
                                        ymax = uci),
                           width = 0.2) +
    ggplot2::geom_point(ggplot2::aes(y = iss)) +
    ggplot2::ylab(id) +
    ggplot2::theme_light() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = NA, colour = NA),
      strip.text.x = ggplot2::element_text(colour = "black"),
      strip.text.y = ggplot2::element_text(colour = "black"),
      panel.border = ggplot2::element_rect(fill = NA),
      legend.key.size = grid::unit(0.9, "lines"),
      legend.key = ggplot2::element_rect(colour = NA, fill = NA),
      legend.background = ggplot2::element_rect(colour = NA, fill = NA)
    )
  
}


#' Adjust axis tick marks and labels
#'
#' @param data = input dataframe
#' @param var = variable of interest e.g., year
#' @param to = step increase desired e.g., every 5 years
#' @param start = adjust the start value
#' @param end = adjust the end vlaue
#' @param min = lowest value to label
#'
tickr <- function (data, var, to = 5, start = NULL, end = NULL, min = NULL)
{
  out <- data %>% dplyr::summarise(min = min({
    {
      var
    }
  }, na.rm = T), max = max({
    {
      var
    }
  }, na.rm = T))
  if (is.null(start) & is.null(end)) {
    data.frame(breaks = out$min:out$max) %>% dplyr::mutate(labels = ifelse(breaks %in%
                                                                             seq(to * min(breaks)/to, max(breaks), by = to), breaks,
                                                                           ""))
  }
  else if (!is.null(start) & is.null(end) & is.null(min)) {
    data.frame(breaks = start:out$max) %>% dplyr::mutate(labels = ifelse(breaks %in%
                                                                           seq(to * start/to, max(breaks), by = to), breaks,
                                                                         ""))
  }
  else if (!is.null(start) & is.null(end) & !is.null(min)) {
    lb <- data.frame(breaks = start:out$max) %>% dplyr::mutate(labels = ifelse(breaks %in%
                                                                                 seq(to * start/to, max(breaks), by = to), breaks,
                                                                               "")) %>% dplyr::filter(breaks >= min)
    lb$labels[1] <- lb$breaks[1]
    lb
  }
  else if (is.null(start) & !is.null(end)) {
    data.frame(breaks = out$min:end) %>% dplyr::mutate(labels = ifelse(breaks %in%
                                                                         seq(to * min(breaks)/to, end, by = to), breaks, ""))
  }
  else {
    data.frame(breaks = start:end) %>% dplyr::mutate(labels = ifelse(breaks %in%
                                                                       seq(to * start/to, end, by = to), breaks, ""))
  }
}

#' Adjust axis tick marks and labels
#'
#' @param data = input dataframe
#' @param var = variable of interest e.g., year
#' @param to = step increase desired e.g., every 5 years
#' @param start = adjust the start value
#' @param end = adjust the end vlaue
#' @param min = lowest value to label
#' @param ... = other scale_x_continuous inputs
#'
scale_x_tickr <- function(..., data, var, to = 5, start=NULL, end=NULL, min=NULL) {
  axis = tickr(data, {{var}}, to, start, end, min)
  ggplot2::scale_x_continuous(breaks = axis$breaks, labels = axis$labels, ...)
}

#' Adjust axis tick marks and labels
#'
#' @param data = input dataframe
#' @param var = variable of interest e.g., year
#' @param to = step increase desired e.g., every 5 years
#' @param start = adjust the start value
#' @param end = adjust the end vlaue
#' @param min = lowest value to label
#' @param ... = other scale_y_continuous inputs
#'
scale_y_tickr <- function(..., data, var, to = 5, start=NULL, end=NULL, min=NULL) {
  axis = tickr(data, {{var}}, to, start, end, min)
  ggplot2::scale_y_continuous(breaks = axis$breaks, labels = axis$labels, ...)
}




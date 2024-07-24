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
  
  get_RSS(species = species,
          region = region,
          comp = comp,
          sex_cat = sex_cat,
          spec_case = spec_case) -> dat1
  
  if(comp == 'length') {
    id = 'Length ISS'
  } else {
    id = 'Age ISS'
  }
  
  dat %>% 
    dplyr::left_join(dat1) %>% 
    dplyr::mutate(Year = factor(year)) %>% 
    ggplot(aes(Year)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = q25th,
                                        ymax = q75th),
                           width = 0.2) +
    ggplot2::geom_point(ggplot2::aes(y = iss)) +
    afscISS::theme_report() +
    ggplot2::ylab(id)
  
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
#' @export tickr
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
#' @export scale_x_tickr
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
#' @export scale_y_tickr
scale_y_tickr <- function(..., data, var, to = 5, start=NULL, end=NULL, min=NULL) {
  axis = tickr(data, {{var}}, to, start, end, min)
  ggplot2::scale_y_continuous(breaks = axis$breaks, labels = axis$labels, ...)
}

#' Set figure theme for reports
#'
#' @param base_size size of font
#' @param base_family font family
#'
#' @export theme_report
#'
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_text
#'
#' @examples
#'

#'theme_report(base_size = 11, base_family = "Times")
#'
#'Other fonts are available, though sans font is
#'the easiest to implement using the following.
#'
#'theme_report(base_family = "")
#'
#'Updating font size is accomplished by changing the base_size.
#'
#'theme_report(base_size = 20, base_family = "")
#'
theme_report <- function(base_size = 11, base_family = "Times") {
  
  windowsFonts(Times=windowsFont("TT Times New Roman"))
  
  half_line <- base_size/2
  
  ggplot2::theme_light(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.ticks.length = grid::unit(half_line / 2.2, "pt"),
      strip.background = ggplot2::element_rect(fill = NA, colour = NA),
      strip.text.x = ggplot2::element_text(colour = "black"),
      strip.text.y = ggplot2::element_text(colour = "black"),
      panel.border = ggplot2::element_rect(fill = NA),
      legend.key.size = grid::unit(0.9, "lines"),
      legend.key = ggplot2::element_rect(colour = NA, fill = NA),
      legend.background = ggplot2::element_rect(colour = NA, fill = NA)
    )
}

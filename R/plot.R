#' Plot Vote Share
#'
#' @param vote_df vote dataframe
#' @param chosen_state chosen state
#'
#' @return vote shares by districts
#' @export
plot_vote_shares <- function(vote_df, chosen_state) {
  
  p <- vote_df %>% 
    filter(state == toupper(chosen_state)) %>% 
    select(year, district, DEMOCRAT_PRCT_TW, REPUBLICAN_PRCT_TW) %>% 
    gather(key = PRCT_TYPE, value = VALUE, -year, -district) %>% 
    mutate(PRCT_TYPE = if_else(PRCT_TYPE == 'REPUBLICAN_PRCT_TW', 'Republican', 'Democratic')) %>% 
    mutate(PRCT_TYPE = factor(PRCT_TYPE, levels = c('Republican', 'Democratic'))) %>% 
    ggplot(aes(x = year, y = VALUE, fill = PRCT_TYPE)) +
    geom_col(width = 7) +
    facet_wrap(~ district) +
    theme_hc() +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    scale_y_continuous(labels = scales::percent) +
    geom_hline(yintercept = 0.5, linetype = 'longdash', color = 'black', size = 0.5) +
    ggtitle(glue('Vote share between Democrats and Republicans - {chosen_state}'))
  
  p
}

#' Plot Two-party Spreads
#'
#' @param chosen_state chosen state
#' @param start_year election start year
#' @param end_year election end year
#' @param vote_df vote data
#' @param tooltips tooltip
#' @param fill_var fill variable
#'
#' @return NULL
#' @export
plots_two_party_spreads <- function(vote_df, chosen_state, start_year = 1976, end_year = 2020, 
                                    tooltips = TRUE, fill_var = 'SPREAD_DR_TW') {
  
  years <- seq(start_year, end_year, 2)
  plots <- map(years, get_two_party_spread_plots, vote_df, chosen_state, tooltips, fill_var = fill_var)
  
  p <- plot_grid(plotlist = plots,
                labels = years,
                ncol = 3)
  
  p
}

#' Get filtered congressional result data
#'
#' @param vote_df data containing congressional vote results
#' @param chosen_state a chosen state
#' @param chosen_year a chosen year
#'
#' @return filtered congressional vote result
#' @export
get_filtered_congress_data <- function(vote_df, chosen_state, chosen_year) {
  
  filtered_data <- vote_df %>% 
    filter(state == toupper(chosen_state),
           year == chosen_year) %>% 
    mutate(dist_name = paste0('Congressional District ', district)) %>% 
    mutate(district = as.character(district))
  
  filtered_data
}

#' Get Plots
#'
#' @param chosen_year a chosen year
#' @param vote_data data containing congressional vote results
#' @param chosen_state a chosen state
#'
#' @return plots
#' @export
get_two_party_spread_plots <- function(chosen_year, vote_data, chosen_state, tooltips = TRUE, fill_var = 'SPREAD_DR_TW') {
  
  mybreaks <- c(-1.5, seq(-0.1750, -0.025, by = 0.025), -0.0001, 0, 0.0001, seq(0.025, 0.1750, by = 0.025), 1.5)
  new_breaks <- seq(mybreaks[1], mybreaks[length(mybreaks)], sect_x(mybreaks))
  my_lims <- c(mybreaks[1], mybreaks[length(mybreaks)])
  rep_times <- (diff(mybreaks)[!is.na(diff(mybreaks))])/sect_x(mybreaks)
  mycols <- c(rev(RColorBrewer::brewer.pal(9, 'Reds')), '#FFFFFF', RColorBrewer::brewer.pal(9, 'Blues'))
  
  vote_data_prc <- get_filtered_congress_data(vote_data, chosen_state, chosen_year)
  
  p <- get_congress_map_by_year(chosen_year) %>% 
    filter(STATENAME == chosen_state) %>% 
    left_join(vote_data_prc, by = c('DISTRICT' = 'district')) %>% 
    ggplot() +
    geom_sf(aes(fill = !!as.name(fill_var))) +
    theme_bw() +
    theme(legend.position = 'none') +
    scale_fill_discrete_gradient(
      limits = my_lims,
      breaks = mybreaks, 
      colors = mycols, 
      bins = length(mycols),
      guide = guide_colourbar(frame.colour = 'black', 
                              ticks.colour = 'black',
                              barwidth = 20)
    )
    
    if (tooltips) {
      p <- p + geom_sf_label_repel(aes(label = paste0('CD ', DISTRICT, ': ', scales::percent(!!as.name(fill_var), 0.01))))
    }
    
  p
}

#' Get Congressional Map based on a congressional year
#'
#' @param cong the specific congress number
#'
#' @return congressional map
#' @export
get_congress_map <- function(cong = 114) {
  tmp_file <- tempfile()
  tmp_dir  <- tempdir()
  zp <- sprintf("http://cdmaps.polisci.ucla.edu/shp/districts%03i.zip",cong)
  download.file(zp, tmp_file)
  unzip(zipfile = tmp_file, exdir = tmp_dir)
  fpath <- paste(tmp_dir, sprintf("districtShapes/districts%03i.shp",cong), sep = "/")
  st_read(fpath)
}

#' Get Congress Number
#'
#' @param election_year election year
#'
#' @return congressional number for which map the election that year is based on.
#' @export
get_congress_number <- function(election_year) {
  
  if (election_year <= 2016) {
    if (election_year %% 2 == 0) {
      congress_start <- election_year - 1
    } else {
      stop('Please enter an even election year')
    }
    
    congress_dif <- (2015 - congress_start) / 2
    congress_num <- 114 - congress_dif
  } else {
    congress_num <- 114
  }
  
  if (congress_num < 114) {
    return(congress_num + 1)
  } else {
    congress_num
  }
}

#' Get Congressional Map by Election Year
#'
#' @param year election year
#'
#' @return congressional map associated with that election year
#' @export
get_congress_map_by_year <- function(year) {
  
  congress_num <- get_congress_number(year)
  get_congress_map(cong = congress_num)
}

#' Get State Names based on Region
#'
#' @param region 
#'
#' @return state names
#' @export
get_states <- function(region) {
  if (region == 'Western') {
    return(c(
      'WA', 'CA', 'OR', 'NV', 'AZ', 'ID', 'UT',
      'CO', 'WY', 'MT', 'NM'
    )) 
  } else if (region == 'Midwest') {
    return(c(
      'ND', 'SD', 'NE', 
      'KS', 'MN', 'IA', 
      'MO', 'WI', 'MI', 
      'IL', 'IN', 'OH'
    )) 
  } else if (region == 'Northeast') {
    return(c(
      'PA', 'NJ', 'NY',
      'CT', 'RI', 'MA',
      'VT', 'NH', 'ME'
    )) 
  } else if (region == 'Southern') {
    return(c(
      'TX', 'OK', 'AR', 'LA',
      'KY', 'TN', 'MS', 'AL'
    ))
  } else if (region == 'South Atlantic') {
    return(c(
      'WV', 'VA', 'DC', 'DE', 'MD',
      'NC', 'SC', 'GA', 'FL'
    ))
  } else if (region == 'Hawaii') {
    return('HI')
  } else if (region == 'Alaska') {
    return('AK')
  }
}

#' Get Plots for Generic Ballot
#'
#' @param chosen_year chosen year
#' @param vote_data vote data
#' @param chosen_region chosen region
#' @param tooltips tooltip
#' @param fill_var fill variable
#'
#' @return generic ballot plot
#' @export
get_two_party_genericbal_plots <- function(chosen_year, vote_data, chosen_region, 
                                           tooltips = TRUE, fill_var = 'generic_dr_spread_tw') {
  
  mybreaks <- c(-1.5, seq(-0.1750, -0.025, by = 0.025), -0.0001, 0, 0.0001, seq(0.025, 0.1750, by = 0.025), 1.5)
  new_breaks <- seq(mybreaks[1], mybreaks[length(mybreaks)], sect_x(mybreaks))
  my_lims <- c(mybreaks[1], mybreaks[length(mybreaks)])
  rep_times <- (diff(mybreaks)[!is.na(diff(mybreaks))])/sect_x(mybreaks)
  mycols <- c(rev(RColorBrewer::brewer.pal(9, 'Reds')), '#FFFFFF', RColorBrewer::brewer.pal(9, 'Blues'))
  
  vote_data_prc <- vote_data %>% 
    filter(year == chosen_year)
  
  p <- tigris::states() %>% 
    filter(STUSPS %in% get_states(chosen_region)) %>% 
    left_join(vote_data_prc, by = c('STUSPS' = 'state_po')) %>% 
    ggplot() +
    geom_sf(aes(fill = !!as.name(fill_var))) +
    theme_bw() +
    theme(legend.position = 'none') +
    scale_fill_discrete_gradient(
      limits = my_lims,
      breaks = mybreaks, 
      colors = mycols, 
      bins = length(mycols),
      guide = guide_colourbar(frame.colour = 'black', 
                              ticks.colour = 'black',
                              barwidth = 20)
    )
  
  if (tooltips) {
    p <- p + geom_sf_label_repel(aes(label = paste0(STUSPS, ': ', scales::percent(generic_dr_spread_tw, 0.01))))
  }
  
  p
}
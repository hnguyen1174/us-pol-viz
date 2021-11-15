#' Get Congressional Vote Data
#' 
#' This function gets and prepares congressional votes data
#' from 1976 to 2020.
#' 
#' Data features returned are:
#' - year, state, state_po (abbreviation), state_fips (FIPS), 
#' - district (number), totalvotes, DEMOCRAT (# of votes), REPUBLICAN (# of votes)
#' - DEMOCRAT_PRCT (%), REPUBLICAN_PRCT (%), SPREAD_DR (DEMOCRAT_PRCT - REPUBLICAN_PRCT)
#' - TOTAL_TW (total two-way), DEMOCRAT_PRCT_TW (democratic percentage two-way %), REPUBLICAN_PRCT_TW
#' - SPREAD_DR_TW (DEMOCRAT_PRCT_TW - REPUBLICAN_PRCT_TW)
#'
#' @param re_process whether to re process the data
#'
#' @return processed congressional vote data
#' @export
get_congressional_vote_data <- function(re_process = FALSE) {
  
  if (re_process) {
    congressional_votes <- readr::read_csv(file.path(here::here(), 'data/1976-2020-house.csv'))
    
    congressional_votes <- congressional_votes %>% 
      select(year, state, state_po, state_fips, district, 
             candidate, party, candidatevotes, totalvotes) %>% 
      filter(!is.na(party)) %>% 
      group_by(year, state, state_po, state_fips, district, party) %>% 
      summarize(candidatevotes = sum(candidatevotes, na.rm = TRUE)) %>% 
      ungroup() %>% 
      left_join(total_votes, by = c('year', 'state', 'district')) %>% 
      spread(key = party, value = candidatevotes) %>% 
      select(year, state, state_po, state_fips, district, totalvotes, DEMOCRAT, REPUBLICAN) %>% 
      mutate(totalvotes = replace_na(totalvotes, 0),
             DEMOCRAT = replace_na(DEMOCRAT, 0),
             REPUBLICAN = replace_na(REPUBLICAN, 0)) %>% 
      mutate(DEMOCRAT_PRCT = DEMOCRAT/totalvotes,
             REPUBLICAN_PRCT = REPUBLICAN/totalvotes,
             SPREAD_DR = DEMOCRAT_PRCT-REPUBLICAN_PRCT,
             SPREAD_RD = REPUBLICAN_PRCT-DEMOCRAT_PRCT,
             TOTAL_TW = DEMOCRAT + REPUBLICAN,
             DEMOCRAT_PRCT_TW = DEMOCRAT/TOTAL_TW,
             REPUBLICAN_PRCT_TW = REPUBLICAN/TOTAL_TW,
             SPREAD_DR_TW = DEMOCRAT_PRCT_TW - REPUBLICAN_PRCT_TW) %>% 
      mutate(DEMOCRAT_PRCT = ifelse(DEMOCRAT_PRCT == Inf, NA, DEMOCRAT_PRCT),
             REPUBLICAN_PRCT = ifelse(REPUBLICAN_PRCT == Inf, NA, REPUBLICAN_PRCT))
    
    write_csv(congressional_votes_total, file.path(here::here(), 'data/processed_congressional_votes.csv'))
  } else {
    congressional_votes <- readr::read_csv(file.path(here::here(), 'data/processed_congressional_votes.csv'))
  }
  
  # Checking ------------------------------------
  test <- congressional_votes %>% 
    group_by(year, state, district) %>% 
    tally() %>% 
    ungroup() %>% filter(n > 1)
  
  if (nrow(test) > 1) stop('An error has occurred.')
  
  congressional_votes
}

#' Get States' Info
#'
#' @return states' info
#' @export
get_states_info <- function() {
  states <- readr::read_csv(file.path(here::here(), 'data/states.csv'))
  states
}

#' Get Generic Ballot Data
#'
#' @param vote_df vote data
#'
#' @return generic ballot data
#' @export
get_generic_ballot_data <- function(vote_df) {
  generic_ballot_data <- vote_df %>% 
    group_by(year, state, state_po) %>% 
    summarize(totalvotes_generic_ballot = sum(totalvotes),
              democrat_generic_ballot = sum(DEMOCRAT),
              republican_generic_ballot = sum(REPUBLICAN),
              total_democrat_republican = democrat_generic_ballot + republican_generic_ballot,
              democrat_generic_prct_tw = democrat_generic_ballot/total_democrat_republican,
              republican_generic_prct_tw = republican_generic_ballot/total_democrat_republican,
              generic_dr_spread_tw = democrat_generic_prct_tw - republican_generic_prct_tw) %>% 
    ungroup()
  
  generic_ballot_data
}
#' Create weights
#' This function will weight the numident file up to HMD lexis triangles
#' Restrict data to deaths from 1988 - 2005 with age_at_death between
#'
#' @param data data.frame with birth and death info
#' @return data.frame
#' @keywords internal
#' @import data.table
#' @export
#'

create_weights_censoc_numident <- function(censoc.numident, cohorts = c(1895:1939), death_ages = c(65:100)) {

  ## Read in death by lexis triangle from HMD
  hmd_deaths <-  fread("/data/josh/CenSoc/hmd/hmd_statistics/deaths/Deaths_lexis/USA.Deaths_lexis.txt") %>%
    mutate(linking_key = paste(Year, Cohort, Age, sep = "_" ))

  ## Calculate counts of deaths in the Numident
  numdeath_aggregate_counts <- censoc.numident %>%
   filter(byear %in% cohorts) %>%
    filter(death_age %in% death_ages) %>%
    group_by(death_age, dyear, byear, sex) %>%
    tally() %>%
    mutate(linking_key = paste(dyear, byear, death_age, sep = "_")) %>%
    ungroup()

  ## Calculate death weights
  death_weights <- numdeath_aggregate_counts %>%
    inner_join(hmd_deaths, by = "linking_key") %>%
    mutate(proportion_matched = case_when(
      sex == 1 ~ n/Male,
      sex == 2 ~ n/Female)) %>%
    group_by(dyear, byear, sex, death_age) %>%
    summarize(inclusion_prob = mean(proportion_matched), Male = mean(Male), Female = mean(Female), Total = mean(Total))

  ## Calculate weights
  death_weights_for_link <-  death_weights %>%
    mutate(linking_key = paste(dyear, byear, death_age, sex, sep = "_")) %>%
    ungroup() %>%
    select(inclusion_prob, linking_key) %>%
    mutate(weight = 1/inclusion_prob) %>%
    select(-inclusion_prob)

  ## create key for link
  censoc.numident <- censoc.numident %>%
    mutate(linking_key = paste(dyear, byear, death_age, sex, sep = "_"))

  ## add weights to original file
  censoc.numident <- censoc.numident %>%
    left_join(death_weights_for_link, by = "linking_key") %>%
    select(-linking_key)

  return(censoc.numident)

}

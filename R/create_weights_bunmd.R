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

create_weights_bunmd <- function(file, death_years = c(1988:2005), death_ages = c(65:100), cohorts = c(1895:1940)) {

  ## read deaths from HMD
  hmd_deaths <-  fread("/global/scratch/p2p3/pl1_demography/censoc/input_data/hmd/hmd_statistics/deaths/Deaths_lexis/USA.Deaths_lexis.txt") %>%
    mutate(linking_key = paste(Year, Cohort, Age, sep = "_" ))

  ## Filter cases (not complete count cases)
  numdeath_aggregate_counts <- file %>%
    filter(dyear %in% death_years) %>%
    filter(death_age %in% death_ages) %>%
    filter(byear %in% cohorts) %>%
    filter(!is.na(sex)) %>%
    group_by(death_age, dyear, byear, sex) %>%
    tally() %>%
    mutate(linking_key = paste(dyear, byear, death_age, sep = "_")) %>%
    ungroup(dyear, death_age)

  ## tabulate deaths
  death_weights <- numdeath_aggregate_counts %>%
    inner_join(hmd_deaths, by = "linking_key") %>%
    mutate(proportion_matched = case_when(
      sex == 1 ~ n/Male,
      sex == 2 ~ n/Female)) %>%
    group_by(dyear, byear, sex, death_age) %>%
    summarize(inclusion_prob = mean(proportion_matched), Male = mean(Male), Female = mean(Female), Total = mean(Total))

  ## create death weights
  death_weights_for_link <-  death_weights %>%
    mutate(linking_key = paste(dyear, byear, death_age, sex, sep = "_")) %>%
    ungroup(dyear, death_age, sex) %>%
    select(inclusion_prob, linking_key) %>%
    mutate(weight = 1/inclusion_prob) %>%
    select(-inclusion_prob) %>%
    ungroup() %>%
    select(-byear)

  ## link to file
  file <- file %>%
    mutate(linking_key = paste(dyear, byear, death_age, sex, sep = "_"))

  file <- file %>%
    left_join(death_weights_for_link, by = "linking_key") %>%
    select(-linking_key)

  return(file)

}

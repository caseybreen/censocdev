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

create_weights_bunmd_complete <- function(file) {

  ## get HMD deaths
  hmd_deaths <-  readHMDweb(CNTRY = "USA", item = "Deaths_lexis", username ="caseybreen@berkeley.edu", password = "censoc") %>%
    mutate(linking_key = paste(Year, Cohort, Age, sep = "_" ))

  ## filter to complete cases
  high_coverage_complete_sample <- file %>%
    filter(dyear %in% c(1988:2005)) %>%
    filter(byear %in% c(1895:1920)) %>%
    filter(death_age %in% c(65:100)) %>%
    filter(!is.na(sex)) %>%
    filter(!is.na(race_last)) %>%
    filter(!is.na(bpl))

  ## tabulate complete cases
  numdeath_aggregate_counts <- high_coverage_complete_sample %>%
    group_by(death_age, dyear, byear, sex) %>%
    tally() %>%
    mutate(linking_key = paste(dyear, byear, death_age, sep = "_")) %>%
    ungroup(dyear, death_age)

  ## create inverse probability weights
  death_weights <- numdeath_aggregate_counts %>%
    inner_join(hmd_deaths, by = "linking_key") %>%
    mutate(proportion_matched = case_when(
      sex == 1 ~ n/Male,
      sex == 2 ~ n/Female)) %>%
    group_by(dyear, byear, sex, death_age) %>%
    summarize(inclusion_prob = mean(proportion_matched), Male = mean(Male), Female = mean(Female), Total = mean(Total))

  ## create death weights
  death_weights_for_link <-  death_weights %>%
    filter(byear %in% c(1895:1940)) %>%
    mutate(linking_key = paste(dyear, byear, death_age, sex, sep = "_")) %>%
    ungroup(dyear, death_age, sex) %>%
    select(inclusion_prob, linking_key) %>%
    mutate(ccweight = 1/inclusion_prob) %>%
    select(-inclusion_prob)

  ## create linking key
  high_coverage_complete_sample <- high_coverage_complete_sample %>%
    mutate(linking_key = paste(dyear, byear, death_age, sex, sep = "_"))

  ## link death weights back to original sample
  weights.df <- high_coverage_complete_sample %>%
    left_join(death_weights_for_link, by = "linking_key") %>%
    select(ssn, ccweight)

  file <- file %>%
    left_join(weights.df, by = "ssn")


  return(file)

}

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

create_weights_dmf_link <- function(file) {

  hmd_deaths <-  readHMDweb(CNTRY = "USA", item = "Deaths_lexis", username = "caseybreen@berkeley.edu", password = "censoc") %>%
    mutate(linking_key = paste(Year, Cohort, Age, sep = "_" ))

  counts <- file %>%
    # filter(dyear %in% c(1988:2005)) %>%
    filter(death_age %in% c(55:105)) %>%
    filter(byear %in% c(1900:1940)) %>%
    group_by(death_age, dyear, byear) %>%
    tally() %>%
    mutate(linking_key = paste(dyear, byear, death_age, sep = "_")) %>%
    ungroup(dyear, death_age)

  death_weights <- counts %>%
    inner_join(hmd_deaths, by = "linking_key") %>%
    mutate(proportion_matched = n/Male) %>%
    group_by(dyear, byear, death_age) %>%
    summarize(inclusion_prob = mean(proportion_matched), Male = mean(Male))

  death_weights_for_link <- death_weights %>%
    filter(byear %in% c(1900:1940)) %>%
    mutate(linking_key = paste(dyear, byear, death_age, sep = "_")) %>%
    ungroup(dyear, death_age) %>%
    select(inclusion_prob, linking_key) %>%
    mutate(weight = 1/inclusion_prob) %>%
    select(-inclusion_prob)

  file <- file %>%
    mutate(linking_key = paste(dyear, byear, death_age, sep = "_"))

  file <- file %>%
    left_join(death_weights_for_link, by = "linking_key") %>%
    select(-linking_key)

  return(file)

}

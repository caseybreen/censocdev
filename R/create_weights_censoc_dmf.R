#########################################
# revised by WON-TAK JOO (2022-11-10)   #
#########################################
#' Create weights
#' This function will weight the numident censoc.dmf up to HMD lexis triangles
#' Restrict data to certain cohorts and ages at deaths
#'
#' @param data data.frame with birth and death info
#' @return data.frame
#' @keywords internal
#' @import data.table
#' @export
#'

create_weights_censoc_dmf <- function(censoc.dmf, cohorts = c(1895:1939), death_ages = c(65:100),
                                      hmd_path = "/data/censoc/input_data/hmd/hmd_statistics/deaths/Deaths_lexis/USA.Deaths_lexis.txt") {

  ## deaths from HMD
  hmd_deaths <-  fread(hmd_path) %>%
    mutate(linking_key = paste(Year, Cohort, Age, sep = "_" ))

  ## create censoc-dmf counts
  ## restrict to certain cohorts, death ages
  counts <- censoc.dmf %>%
    filter(byear %in% cohorts) %>%
    filter(death_age %in% death_ages) %>%
    group_by(death_age, dyear, byear) %>%
    tally() %>%
    mutate(linking_key = paste(dyear, byear, death_age, sep = "_")) %>%
    ungroup(dyear, death_age)

  ## join censoc-dmf counts and HMD count
  death_weights <- counts %>%
    inner_join(hmd_deaths, by = "linking_key") %>%
    mutate(proportion_matched = n/Male) %>%
    group_by(dyear, byear, death_age) %>%
    summarize(inclusion_prob = mean(proportion_matched), Male = mean(Male), .groups = 'drop')

  ## divide death weights
  death_weights_for_link <- death_weights %>%
    mutate(linking_key = paste(dyear, byear, death_age, sep = "_")) %>%
    select(inclusion_prob, linking_key) %>%
    mutate(weight = 1/inclusion_prob) %>%
    select(-inclusion_prob)

  ## create linking key
  censoc.dmf <- censoc.dmf %>%
    mutate(linking_key = paste(dyear, byear, death_age, sep = "_"))

  ## add weights onto censoc-dmf censoc.dmf
  censoc.dmf <- censoc.dmf %>%
    left_join(death_weights_for_link, by = "linking_key") %>%
    select(-linking_key)

  return(censoc.dmf)

}

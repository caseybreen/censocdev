####################################################
# Last revised by Maria Osborne (August 2023)   #
####################################################
#' Master Functions for creating CenSoc Weights with
#' NCHS data:
#'
#' create_NCHS_weights_numident()
#' create_NCHS_weights_dmf()


# Source smaller functions
source("~/CenSoc Weights/code/nchs_weighting_functions.R")


# Function for weighting Numident records
create_NCHS_weights_numident <- function(censoc_data, # tabulated CenSoc-Numident data
                                         tabulated_population_data, # tabulated population data
                                         end_year = 2005, # last year to compute weights for
                                         verbose = TRUE, # print progress messages
                                         debug = TRUE) { # print checks

  # filter tabulated population data
  start_year <- 1988
  tabulated_population_data <- tabulated_population_data[year %in% start_year:end_year]

  # compute basic inverse probability weights for the US born
  if(verbose) {print("====== computing raw weights  ====== ")}
  unadjusted_us_born_weights <- compute_basic_weights(censoc_data, tabulated_population_data)

  # fill weights for anyone not weighted previously (missing bpl, foreign born, etc.)
  if(verbose) {print("====== filling weights ======")}
  unadjusted_filled_weights <- fill_weights(censoc_data, unadjusted_us_born_weights)

  if (debug) {
    # check that we have assigned an unadjusted weight to all rows
    nrow(unadjusted_us_born_weights[!is.na(ps_weight)] %>% uncount(weights = sample_n)) +
      nrow((unadjusted_filled_weights %>% uncount(weights = sample_n))) ==
      nrow(censoc_data %>% uncount(weights = sample_n)) # should be TRUE
    # check that no strata has been assigned both kinds of weights
    intersect(unadjusted_us_born_weights[!is.na(ps_weight)]$key, unadjusted_filled_weights$key) # should be character(0)
    # check that all strata have been assigned an unadjusted weight
    setequal(union(unadjusted_us_born_weights[!is.na(ps_weight)]$key, unadjusted_filled_weights$key),
             censoc_data$key) # should be TRUE
    # check that all NA us born-weights were assigned a filled weight
    all(unadjusted_us_born_weights[is.na(ps_weight)]$key %in% unadjusted_filled_weights$key) # should be TRUE
  }

  # combine into one dataset
  all_unadjusted_weights <- plyr::rbind.fill(unadjusted_us_born_weights[!is.na(ps_weight)],
                                             unadjusted_filled_weights) %>% as.data.table()
  all_unadjusted_weights[!is.na(ps_weight), unadj_weight := ps_weight]
  all_unadjusted_weights[!is.na(mean_wt), unadj_weight := mean_wt]

  # Adjust weights for the us-born by raking and trimming
  if(verbose) {print("====== starting weight raking ======")}
  adjusted_us_born_weights <- rake_weights_numident(all_unadjusted_weights, tabulated_population_data)
  if(verbose) {print("====== weight raking complete ======")}

  # adjust remaining weights by trimming
  if(verbose) {print("====== making final adjustments ======")}
  adjusted_other_weights <- adjust_other_weights_numident(all_unadjusted_weights)

  # combine all weights
  all_weights <- plyr::rbind.fill(adjusted_us_born_weights, adjusted_other_weights) %>%
    as.data.table()
  all_weights[!is.na(raked_weight), weight_final := raked_weight]
  all_weights[!is.na(adjusted_wt), weight_final := adjusted_wt]

  if (debug) {
    #check
    nrow(all_weights) == nrow(censoc_data)
    setequal(all_weights$key, censoc_data$key)
    all_weights[is.na(weight_final)]
  }

  return(all_weights[, .(key, weight_final)])
}




# Function for weighting CenSoc-DMF data
create_NCHS_weights_dmf <- function(censoc_data, # tabulated censoc-DMF data
                                    tabulated_population_data, # tabulated NCHS data
                                    end_year = 2005, # last year to comput weights for
                                    verbose = TRUE, # print progess messags
                                    debug = TRUE) { # print checks
  # filter population data to select years and men only
  start_year <- 1979
  tabulated_population_data <- tabulated_population_data[year %in% start_year:end_year]
  tabulated_population_data <- tabulated_population_data[sex_string == "male"]

  # compute basic inverse probability weights for the US born 1979 onward
  if(verbose) {print("====== computing raw weights ====== ")}
  unadjusted_us_born_weights <- compute_basic_weights(censoc_data[dyear >= 1979], tabulated_population_data)

  # extend basic weights backwards for 1975-1978
  if(verbose) {print("====== extrapolating weights for 1975-1978 ======")}
  unadjusted_extended_weights <- extend_dmf_weights_backwards(censoc_data, unadjusted_us_born_weights)

  # combine these two kinds of weights
  unadjusted_us_weights_all_years <- plyr::rbind.fill(unadjusted_us_born_weights,
                                                      unadjusted_extended_weights %>% dplyr::select(-c(year_weight_from))) %>%
    as.data.table()

  # fill weights for anyone not weighted previously (foreign born, etc.)
  if(verbose) {print("====== filling weights ======")}
  unadjusted_filled_weights <- fill_weights(censoc_data, unadjusted_us_born_weights = unadjusted_us_weights_all_years)

  if (debug) {
    # check that we have assigned an unadjusted weight to all rows
    nrow(unadjusted_us_weights_all_years[!is.na(ps_weight)] %>% uncount(weights = sample_n)) +
      nrow((unadjusted_filled_weights %>% uncount(weights = sample_n))) ==
      nrow(censoc_data %>% uncount(weights = sample_n)) # should be TRUE
    # check that no strata has been assigned both kinds of weights
    intersect(unadjusted_us_weights_all_years[!is.na(ps_weight)]$key, unadjusted_filled_weights$key) # should be character(0)
    # check that all strata have been assigned an unadjusted weight
    setequal(union(unadjusted_us_weights_all_years[!is.na(ps_weight)]$key, unadjusted_filled_weights$key),
             censoc_data$key) # should be TRUE
    # check that all NA US-born-weights were assigned a filled weight
    all(unadjusted_us_born_weights[is.na(ps_weight)]$key %in% unadjusted_filled_weights$key) # should be TRUE
  }

  #combine into one dataset
  all_unadjusted_weights <- plyr::rbind.fill(unadjusted_us_weights_all_years[!is.na(ps_weight)],
                                             unadjusted_filled_weights) %>% as.data.table()
  all_unadjusted_weights[!is.na(ps_weight), unadj_weight := ps_weight]
  all_unadjusted_weights[!is.na(mean_wt), unadj_weight := mean_wt]

  # rake weights for the US-born 1979 onwards
  if(verbose) {print("====== starting weight raking ======")}
  adjusted_us_born_weights <- rake_weights_dmf(all_unadjusted_weights, tabulated_population_data)
  if(verbose) {print("====== weight raking complete ======")}

  # adjust other weights (non-US born 1979+, and all birthplaces pre-1979) just by trimming
  adjusted_other_weights <- adjust_other_weights_dmf(all_unadjusted_weights)
  if(verbose) {print("====== final adjustments complete ======")}

  # combine all weights
  all_weights <- plyr::rbind.fill(adjusted_us_born_weights, adjusted_other_weights) %>%
    as.data.table()
  all_weights[!is.na(raked_weight), weight_final := raked_weight]
  all_weights[!is.na(adjusted_wt), weight_final := adjusted_wt]

  if (debug) {
    # check to make sure nothing is missing/duplicated
    nrow(all_weights) == nrow(censoc_data)
    setequal(all_weights$key, censoc_data$key)
    all_weights[is.na(weight_final)]
  }
  return(all_weights[, .(key, weight_final)])
}

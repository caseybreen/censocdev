####################################################
# Last revised by Maria Osborne (August 15 2023)   #
####################################################
#' Functions for weighting and adjusting CenSoc data
#' using NCHS population totals
#' 
#' compute_basic_weights() : compute inverse probability weights
#' for US-born people dying 1979 and later
#' 
#' extend_dmf_weights_backwards() : weight DMF 1975-1978
#' 
#' fill_weights() : compute filled weights for people born outside
#' the US, with unknown birthplace, or born in the US but not otherwise weightable
#' 
#' rake_weights_numident() / rake_weights_dmf() : For people born in the US
#' 1979 and later, calibrate marginal weighted totals to agree with population
#' marginal totals, and constrain weights to a set min and max
#' 
#' adjust_other_weights_numident() / adjust_other_weights_dmf(): for people
#' NOT born in the US and/or dying before 1979, adjust weights by constraining to 
#' a set min and max


## Function for creating basic weights for people
## Born in the contiguous United States
compute_basic_weights <- function(censoc_data, tabulated_population_data) {
  censoc_data_us <- censoc_data[bpl_flag_usa == 1]
  # join tabulated data
  joined_full <- as.data.table(left_join(censoc_data_us, tabulated_population_data[,.(population_n, key)], by = "key"))
  
  # compute weights 
  joined_full[, ps_weight := population_n/sample_n]
  
  return(joined_full)
}


# Function for creating filled weights for anyone:
# 1) born outside the US, including territories, Alaska, Hawaii,
# 2) Have unknown birthplaces
# 3) Born in US, belong to strata not present in NCHS data.
# Assign mean of US-born people with same characteristics (sex, age, year, race)
# Assign a weight of 1 if this fails
fill_weights <- function(censoc_data, unadjusted_us_born_weights, verbose = TRUE) {
  # averages will be taken among
  domestic_weighted <- unadjusted_us_born_weights[bpl_flag_usa == 1 & !is.na(ps_weight)]
  
  # take averages by age/year/sex/race
  domestic_weighted_uncounted <- domestic_weighted %>% uncount(weights = sample_n, .remove=FALSE)
  domestic_means <- domestic_weighted_uncounted %>% group_by(sex_string, census_race, dyear, death_age) %>% 
    summarize(mean_wt = mean(ps_weight)) %>% 
    as.data.table()
  
  # define who we need to give a weight to:
  # Alaska/Hawaii, unspecified US, US-born people
  # who weren't assigned a weight for some reason
  us_born_missing_strata <- unadjusted_us_born_weights[is.na(ps_weight)]$key
  strata_to_fill <- censoc_data[bpl_flag_AKHI == 1 | bpl_flag_foreign == 1 |
                                  bpl_flag_missing == 1 | bpl_flag_territory == 1 |
                                  (key %in% us_born_missing_strata)]
  
  filled_weights <- left_join(strata_to_fill, domestic_means[, .(sex_string, census_race, dyear, death_age,
                                                                 mean_wt)],
                              by = c("sex_string", "census_race", "dyear", "death_age"))
  
  # fill remaining NA's with ones
  if(verbose) {
    print(paste( nrow(filled_weights[is.na(mean_wt)]), "strata (", sum(filled_weights[is.na(mean_wt)]$sample_n),   
           "records) could not be filled and were set to one"))
  }
  filled_weights[is.na(mean_wt), mean_wt := 1]
  
  return(filled_weights)
}


# Function to rake and trim Numident Weights
rake_weights_numident <- function(all_unadjusted_weights, tabulated_population_data,
                                  verbose = TRUE, debug = TRUE) {
  # Only calibrate records for US-born
  numident_weights <- all_unadjusted_weights[bpl_flag_usa == 1]
  numident_weights_uncounted <- numident_weights %>% uncount(weights = sample_n, .remove = FALSE)
  numident_weights_uncounted[, dyear := as.factor(dyear)]
  numident_weights_uncounted[, death_age := as.factor(death_age)]
  
  # Set parameters for raking/trimming
  max_weight <- mean(numident_weights_uncounted$unadj_weight)*5
  min_weight <-  1
  tolerance <- 0.0001 # calibrate until highest weight is within this distance of max_weight
  if(debug) {
    print(paste("maximum weight set to", round(max_weight,3)))
  }
  
  
  # Get Population marginal totals
  all_strata <- tabulated_population_data[bpl_flag_usa == 1]
  race_marginals <- all_strata %>% 
    group_by(race_string) %>% 
    summarize(n = sum(population_n)) %>%
    pivot_wider(names_from = race_string,
                values_from = n,
                values_fill = 0,
                names_prefix = "census_race") 
  
  sex_marginals <- all_strata %>% 
    group_by(sex_string) %>% 
    summarize(n = sum(population_n)) %>%
    pivot_wider(names_from = sex_string,
                values_from = n,
                values_fill = 0, 
                names_prefix = "sex_string") 
  
  age_marginals <- all_strata %>%
    group_by(death_age_years) %>%
    summarize(n = sum(population_n)) %>%
    pivot_wider(names_from = death_age_years,
                values_from = n,
                values_fill = 0,
                names_prefix = "death_age")
  
  year_marginals <- all_strata %>%
    group_by(year) %>%
    summarize(n = sum(population_n)) %>%
    pivot_wider(names_from = year,
                values_from = n,
                values_fill = 0,
                names_prefix = "dyear")
  
  bpl_marginals <- all_strata %>%
    group_by(bpl_key) %>%
    summarize(n = sum(population_n)) %>%
    pivot_wider(names_from = bpl_key,
                values_from = n,
                values_fill = 0,
                names_prefix = "bpl_key")
  
  population_marginal_totals <- cbind(race_marginals, sex_marginals,
                                      year_marginals, bpl_marginals, age_marginals)
  

  
  # get the (unweighted) CenSoc totals in form of a model matrix
  # (it's not rally a model matrix, as it doesn't drop a level from the categorical vars)

  # we don't have enough RAM to do this in one fell swoop, so will make matrices by
  # each variable and then attach them
  #dummy_vars <- dummyVars( ~ census_race + sex_string + dyear + bpl_key + death_age,
  #                         data = numident_weights_uncounted,
  #                         sep = NULL)
  #dummy_mat <- data.frame(predict(dummy_vars, newdata = numident_weights_uncounted)) 
  
  dv_year <- dummyVars(~ dyear, data = numident_weights_uncounted, sep = NULL)
  mat_year <- data.frame(predict(dv_year, newdata = numident_weights_uncounted))
  dv_age <- dummyVars(~ death_age, data = numident_weights_uncounted, sep = NULL)
  mat_age <- data.frame(predict(dv_age, newdata = numident_weights_uncounted))
  dv_bpl <- dummyVars(~ bpl_key, data = numident_weights_uncounted, sep = NULL)
  mat_bpl <- data.frame(predict(dv_bpl, newdata = numident_weights_uncounted))
  dv_othervars <- dummyVars(~ census_race + sex_string, data = numident_weights_uncounted, sep = NULL)
  mat_othervars <- data.frame(predict(dv_othervars, newdata = numident_weights_uncounted))
  dummy_mat <- cbind(mat_othervars, mat_year, mat_bpl, mat_age)
  rm(mat_age, mat_bpl, mat_year, mat_othervars)
  gc()
  
  
  temp <- numident_weights_uncounted
  rm(numident_weights_uncounted)
  gc()

  if (debug) {
    # check that the sample matrix has same properties as the population marginal data structure
    length(population_marginal_totals) == ncol(dummy_mat)
    # variables must be in exact same order
    mean(names(population_marginal_totals) == names(dummy_mat)) == 1
  }   
  
  ##### Loop ########
  # initial values
  temp[, wt_to_adjust := unadj_weight]
  temp[ wt_to_adjust > max_weight, wt_to_adjust := max_weight]
  temp[ wt_to_adjust < min_weight, wt_to_adjust := min_weight]
  d <- temp$wt_to_adjust
  Xs <- as.matrix(dummy_mat)
  rm(dummy_mat)
  gc()
  
  iters <- 25 # max number of times to run the loop, but should stop much earlier
  
  for(i in 1:iters) {
    gs <- calib(Xs = Xs, # model matrix
                d = d, # unadjusted weights
                total = t(as.matrix(population_marginal_totals)), # pop marginal totals
                method = "raking", # method                         
                q=rep(1, nrow(temp))) # heteroskedasticity thing,,
    
    
    # check the calibration
    check <- checkcalibration(Xs = Xs,
                              d= d,
                              total = t(as.matrix(population_marginal_totals)),
                              g = gs,
                              EPS = 1e-6)
    
    if (verbose) {
      # Check that the model converged
      print(paste("iteration no.", i))
      print("calibration results:")
      print(check)
      
      adj_wts <- gs * d
      print("summary of adjusted weights")
      print(summary(adj_wts))
      
      print(paste('Distance of highest weight from maximum allowable weight:', abs(max_weight - max(adj_wts))))
      print("__________")
    }  
    
    if(abs(max_weight - max(adj_wts)) < tolerance) {
      print(paste0("reached acceptable bounds in ", i, " iterations"))
      break
    }
    
    # trim and go again 
    adj_wts[adj_wts > max_weight] <- max_weight
    adj_wts[adj_wts < min_weight] <- min_weight
    
    d <- adj_wts
    
    if (i == iters) {
      print(paste0("did not reached acceptable bounds within ", i, " iterations"))
    }
  }
  
  # Reattach adjusted weights to data
  temp$raked_weight <- adj_wts
  
  if (debug) {
    # check that the totals are kosher
    temp %>% group_by(census_race) %>% 
      summarize(weighted_total = sum(raked_weight))
    race_marginals
    
    temp %>% group_by(sex_string) %>% 
      summarize(weighted_total = sum(raked_weight))
    sex_marginals
    
    temp %>% group_by(death_age) %>% 
      summarize(weighted_total = sum(raked_weight)) %>% 
      cbind((cbind(name = names(age_marginals), count = t(age_marginals[1,]))))
    
    temp %>% group_by(dyear) %>% 
      summarize(weighted_total = sum(raked_weight)) %>% 
      cbind((cbind(name = names(year_marginals), count = t(year_marginals[1,]))))
  }
  
  # return raked weights by strata
  return(temp %>% distinct(key, raked_weight, .keep_all = TRUE))
}




# function to rake and trim DMF weights
rake_weights_dmf <- function(all_unadjusted_weights, tabulated_population_data,
                             verbose = TRUE, debug = TRUE) {
  # Only calibrate records for US-born years 1979 +
  dmf_weights <- all_unadjusted_weights[bpl_flag_usa == 1 & dyear >= 1979]
  dmf_weights_uncounted <- dmf_weights %>% uncount(weights = sample_n, .remove = FALSE)
  
  # Set parameters for raking/trimming
  max_weight <- mean(dmf_weights_uncounted$unadj_weight)*5
  min_weight <-  1
  tolerance <- 0.0001 # calibrate until highest weight is within this distance of max_weight
  if(debug) {
    print(paste("maximum weight set to", round(max_weight,3)))
  }
  
  
  # Get Population marginal totals
  all_strata <- tabulated_population_data[bpl_flag_usa == 1]
  race_marginals <- all_strata %>% 
    group_by(race_string) %>% 
    summarize(n = sum(population_n)) %>%
    pivot_wider(names_from = race_string,
                values_from = n,
                values_fill = 0,
                names_prefix = "census_race") 
  
  age_marginals <- all_strata %>%
    group_by(death_age_years) %>%
    summarize(n = sum(population_n)) %>%
    pivot_wider(names_from = death_age_years,
                values_from = n,
                values_fill = 0,
                names_prefix = "death_age")
  
  year_marginals <- all_strata %>%
    group_by(year) %>%
    summarize(n = sum(population_n)) %>%
    pivot_wider(names_from = year,
                values_from = n,
                values_fill = 0,
                names_prefix = "dyear")
  
  bpl_marginals <- all_strata %>%
    group_by(bpl_key) %>%
    summarize(n = sum(population_n)) %>%
    pivot_wider(names_from = bpl_key,
                values_from = n,
                values_fill = 0,
                names_prefix = "bpl_key")
  
  population_marginal_totals <- cbind(race_marginals, year_marginals, bpl_marginals, age_marginals)
  
  # get the (unweighted) CenSoc totals in form of a model matrix
  # (it's not rally a model matrix, as it doesn't drop a level from the categorical vars)
  dmf_weights_uncounted[, dyear := as.factor(dyear)]
  dmf_weights_uncounted[, death_age := as.factor(death_age)]
  dummy_vars <- dummyVars( ~ census_race + dyear + bpl_key + death_age,
                           data = dmf_weights_uncounted,
                           sep = NULL)
  dummy_mat <- data.frame(predict(dummy_vars, newdata = dmf_weights_uncounted)) 
  
  if (debug) {
    # check that the sample matrix has same properties as the population marginal data structure
    length(population_marginal_totals) == ncol(dummy_mat)
    mean(names(population_marginal_totals) == names(dummy_mat)) == 1
  }   
  
  ##### Loop ########
  # initial values
  temp <- dmf_weights_uncounted
  temp[, wt_to_adjust := unadj_weight]
  temp[ wt_to_adjust > max_weight, wt_to_adjust := max_weight]
  temp[ wt_to_adjust < min_weight, wt_to_adjust := min_weight]
  d <- temp$wt_to_adjust
  
  iters <- 25 # max number of times to run the loop, but should stop much earlier
  
  for(i in 1:iters) {
    gs <- calib(Xs = as.matrix(dummy_mat), # model matrix
                d = d, # unadjusted weights
                total = t(as.matrix(population_marginal_totals)), # pop marginal totals
                method = "raking", # method                         
                q=rep(1, nrow(temp))) # heteroskedasticity thing,,
    
    
    # check the calibration
    check <- checkcalibration(Xs = as.matrix(dummy_mat),
                              d= d,
                              total = t(as.matrix(population_marginal_totals)),
                              g = gs,
                              EPS = 1e-6)
    
    if (verbose) {
      # Check that the model converged
      print(paste("iteration no.", i))
      print("calibration results:")
      print(check)
      
      adj_wts <- gs * d
      print("summary of adjusted weights")
      print(summary(adj_wts))
      
      print(paste('Distance of highest weight from maximum allowable weight:', abs(max_weight - max(adj_wts))))
      print("__________")
    }  
    
    if(abs(max_weight - max(adj_wts)) < tolerance) {
      print(paste0("reached acceptable bounds in ", i, " iterations"))
      break
    }
    
    # trim and go again 
    adj_wts[adj_wts > max_weight] <- max_weight
    adj_wts[adj_wts < min_weight] <- min_weight
    
    d <- adj_wts
    
    if (i == iters) {
      print(paste0("did not reached acceptable bounds within ", i, " iterations"))
    }
  }
  
  # Reattach adjusted weights to data
  temp$raked_weight <- adj_wts
  
  if (debug) {
    # manually inspect raked marginal totals to see if they align with true pop totals
    temp %>% group_by(census_race) %>% 
      summarize(weighted_total = sum(raked_weight))
    race_marginals
    
    temp %>% group_by(death_age) %>% 
      summarize(weighted_total = sum(raked_weight)) %>% 
      cbind((cbind(name = names(age_marginals), count = t(age_marginals[1,]))))
    
    temp %>% group_by(dyear) %>% 
      summarize(weighted_total = sum(raked_weight)) %>% 
      cbind((cbind(name = names(year_marginals), count = t(year_marginals[1,]))))
    
    temp %>% group_by(bpl_key) %>% 
      summarize(weighted_total = sum(raked_weight)) %>% 
      cbind((cbind(name = names(bpl_marginals), count = t(bpl_marginals[1,]))))
  }

  # return raked weights by strata
  return(temp %>% distinct(key, raked_weight, .keep_all = TRUE))
  
}





# Function for getting 1975-1978 weights using 
# weights of same strata in 1979
extend_dmf_weights_backwards <- function(censoc_data, unadjusted_us_born_weights) {
  # get weights in 1979
  dmf_weights_real <- unadjusted_us_born_weights[dyear == 1979 & !is.na(ps_weight)]
  
  # select and rename vars to match across datasets
  dmf_weights_real[, year_weight_from := dyear]
  dmf_weights_real <- dmf_weights_real[, .(year_weight_from, death_age, sex_string, census_race, bpl_key, ps_weight)]
  
  
  # Attach 1979 weights to previous years
  dmf_pre79_dat <- censoc_data[dyear %in% 1975:1978 & bpl_flag_usa == 1]
  dmf_pre79_dat <- left_join(dmf_pre79_dat, dmf_weights_real[year_weight_from == 1979],
                             by = c("death_age", "census_race", "sex_string", "bpl_key"))
  
  # Return
  return(dmf_pre79_dat)
}



# Function to adjust weights that are not affecting by raking
# (People born outside the United States)
adjust_other_weights_numident <- function(all_unadjusted_weights) {
  # Set min and max for trimming
  ua_weights_uncounted <- all_unadjusted_weights[bpl_flag_usa == 1] %>%
    uncount(weights = sample_n, .remove = FALSE)
  max_weight <- mean(ua_weights_uncounted$unadj_weight)*5
  min_weight <-  1
  
  to_adjust <- all_unadjusted_weights[bpl_flag_usa == 0]
  to_adjust[unadj_weight > max_weight, adjusted_wt :=  max_weight] 
  to_adjust[unadj_weight < min_weight, adjusted_wt :=  min_weight]
  to_adjust[unadj_weight >= min_weight & unadj_weight <= max_weight, adjusted_wt :=  unadj_weight] 
  
  rm(ua_weights_uncounted)
  return(to_adjust)
}

# Function to adjust weights that are not affecting by raking
# (People born outside the United States)
adjust_other_weights_dmf <- function(all_unadjusted_weights) {
  # Set min and max for trimming
  ua_weights_uncounted <- all_unadjusted_weights[bpl_flag_usa == 1] %>%
    uncount(weights = sample_n, .remove = FALSE)
  max_weight <- mean(ua_weights_uncounted$unadj_weight)*5
  min_weight <-  1
  
  to_adjust <- all_unadjusted_weights[bpl_flag_usa == 0 | dyear < 1979]
  to_adjust[unadj_weight > max_weight, adjusted_wt :=  max_weight] 
  to_adjust[unadj_weight < min_weight, adjusted_wt :=  min_weight]
  to_adjust[unadj_weight >= min_weight & unadj_weight <= max_weight, adjusted_wt :=  unadj_weight] 
  
  return(to_adjust)
}

####################################################
# Last revised by Maria Osborne (August 15 2023)   #
####################################################
#' Prepare DMF Data for weighting
#'
#' Harmonize and tabulate data for all years
#'
#' To weight:
#' 1) conservative links
#' 2) deaths age 65-100
#' 3) years 1975-2005
#'
#' Save individuals records and tabulated records by strata
#'

# libraries
library(data.table)
library(tidyverse)

# Input paths
path_dmf <- "/global/scratch/p2p3/pl1_demography/censoc/censoc_data_releases/censoc_linked_to_census/v2.1/censoc_dmf_v2.1_linked.csv"
path_us_bpl_codes <- "/global/scratch/p2p3/pl1_demography/censoc_internal/censoc_weights/data/IPUMS_detailed_BPL.txt"
path_NCHS_data <- "/global/scratch/p2p3/pl1_demography/censoc_internal/censoc_weights/data/NCHS/NCHS_data_harmonized.csv"

# Output paths
path_out <- "/global/scratch/p2p3/pl1_demography/censoc_internal/censoc_weights/data/DMF/"

# Read DMF
dmf_vars <- c("HISTID", "death_age", "byear", "dyear", "SEX", "BPLD", "RACED",
              "link_abe_exact_conservative")
censoc_dmf <- fread(path_dmf, select = dmf_vars)

## Select ages and years to weight
# birthplace availibility starts in 1979, so we won't weight death years prior.
dmf_to_weight <- censoc_dmf[death_age %in% 65:100 & dyear %in% 1975:2005]

# Limit to conservative matches
dmf_to_weight <- dmf_to_weight[link_abe_exact_conservative == 1]

## harmonize variables to match processed NCHS data
# sex string
dmf_to_weight[, sex_string := "male"]

# race string (use census race)
dmf_to_weight[RACED == 100, census_race := "white"]
dmf_to_weight[RACED == 200, census_race := "black"]
dmf_to_weight[RACED != 100 & RACED != 200, census_race := "other"]

# Birthplace strings
# USA: translate codes to strings
us_bpls <- fread(path_us_bpl_codes)
us_bpls <- us_bpls[code <= 05610]
colnames(us_bpls) = c("BPLD", "usa_bpl_string")
dmf_to_weight <- left_join(dmf_to_weight, us_bpls, by = "BPLD")
dmf_to_weight[BPLD <= 05610, bpl_temp := usa_bpl_string]

# unspecified US birthplaces
dmf_to_weight[BPLD %in% c(9000,9900), bpl_temp := "Unspecified United States"] # us states

# recode "Indian Territory" to Oklahoma and "Dakota Territory" to South Dakota
dmf_to_weight[BPLD == 4010, bpl_temp := "Oklahoma"]
dmf_to_weight[BPLD == 4610, bpl_temp := "South Dakota"]

# Current U.S. territories: lump together other than PR
dmf_to_weight[BPLD == 11000, bpl_temp := "Puerto Rico"]
dmf_to_weight[BPLD %in% c(10000:10500, 11500:12092), bpl_temp := "Other US Territory"]

# select foreign nations
dmf_to_weight[BPLD %in% 15000:15083, bpl_temp := "Canada"]
dmf_to_weight[BPLD == 20000, bpl_temp := "Mexico"]
dmf_to_weight[BPLD == 25000, bpl_temp := "Cuba"]

# all other foreign born
dmf_to_weight[BPLD > 15083 & BPLD != 20000 & BPLD != 25000 & BPLD < 90000,
              bpl_temp := "Other Foreign Born"]

# Above 90000 is unspecified abroad or at sea
dmf_to_weight[BPLD >= 90000, bpl_temp := "Unknown Abroad"]

table(dmf_to_weight$bpl_temp, useNA = "always") # make sure nothing here looks weird. no NAs.

# translate bpl strings to lowercase keys for matching
dmf_to_weight[, bpl_key := bpl_temp]
dmf_to_weight[, bpl_key := tolower(gsub(" ", "", bpl_key))] # remove spaces and capitals


# create flags for types of birthplaces
dmf_to_weight[, bpl_flag_usa := as.integer(BPLD <= 05610 & BPLD != 1500 & BPLD != 200)] # contiguous usa
dmf_to_weight[, bpl_flag_territory := as.integer(BPLD %in% 10000:12092)] # current territories, like P.R.
dmf_to_weight[, bpl_flag_AKHI := as.integer(BPLD %in% c(1500, 200))] # alaska & hawaii
dmf_to_weight[, bpl_flag_foreign := as.integer(BPLD > 12092 & BPLD < 90000)] # foreign nations
dmf_to_weight[, bpl_flag_missing :=  as.integer(bpl_temp %in% c("Unknown Abroad", "Unspecified United States"))] #missing

# make sure everything has been assigned a flag
# (this should add up to one)
mean(dmf_to_weight$bpl_flag_AKHI==1) + mean(dmf_to_weight$bpl_flag_usa==1) +
  mean(dmf_to_weight$bpl_flag_territory==1) + mean(dmf_to_weight$bpl_flag_foreign==1) +
  mean(dmf_to_weight$bpl_flag_missing==1)

# create keys
dmf_to_weight[, key := paste(paste0("a", death_age),
                             paste0("y", dyear),
                             paste0("s", sex_string),
                             paste0("r", census_race),
                             paste0("b", bpl_key),
                             sep = "_")]

# save this dataset
write_csv(dmf_to_weight,
          paste0(path_out, "dmf_data_prepped.csv"))


# Tabulate
dmf_tab <-dmf_to_weight %>%
  group_by(death_age, dyear, sex_string, census_race, bpl_key,
           bpl_flag_usa, bpl_flag_territory, bpl_flag_AKHI, bpl_flag_foreign,
           bpl_flag_missing, key) %>%
  summarize(sample_n = n())

# check this is ok
length(unique(dmf_tab$key)) == nrow(dmf_tab) # no repeat keys in the tabulated data
length(unique(dmf_tab$key)) == length(unique(dmf_to_weight$key)) # all keys captured

# save
write_csv(dmf_tab,
          paste0(path_out, "dmf_data_prepared_tabulated.csv"))

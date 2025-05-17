####################################################
# Last revised by Maria Osborne (August 14 2023)   #
####################################################
#' Prepare Numident Data for weighting
#'
#' Harmonize and tabulate data for all years
#'
#' To weight:
#' 1) conservative links
#' 2) deaths age 65-100
#' 3) years 1988-2005
#'
#' Save individuals records and tabulated records by strata
#'

# libraries
library(data.table)
library(tidyverse)

# Input paths
path_numident <- "/global/scratch/p2p3/pl1_demography/censoc/censoc_data_releases/censoc_linked_to_census/v2.1/censoc_numident_v2.1_linked.csv"
path_us_bpl_codes <- "/global/scratch/p2p3/pl1_demography/censoc_internal/censoc_weights/data/IPUMS_detailed_BPL.txt"

# Output paths
path_out <- "/global/scratch/p2p3/pl1_demography/censoc_internal/censoc_weights/data/numident/"

# Read data
numident_vars <- c("HISTID", "death_age", "byear", "dyear", "sex", "bpl", "bpl_string",
                   "RACED", "link_abe_exact_conservative", "BPLD")
censoc_numident <- fread(path_numident, select = numident_vars)

# select conservative only, appropriate years and ages
censoc_numident_toweight <- censoc_numident[link_abe_exact_conservative == 1 &
                                              death_age %in% 65:100 & dyear %in% 1988:2005]

# harmonize variables to match processed NCHS data
# sex string
censoc_numident_toweight[sex == 1, sex_string := "male"]
censoc_numident_toweight[sex == 2, sex_string := "female"]

# race string (use census race, not numident)
censoc_numident_toweight[RACED == 100, census_race := "white"]
censoc_numident_toweight[RACED == 200, census_race := "black"]
censoc_numident_toweight[RACED != 100 & RACED != 200, census_race := "other"]

# Translate Birthplaces into categories
# Use Census BPL, not numident bpl : numident bpl has a lot of "missing" bpls
# that are actually valid countries. For instance, over 39k people
# are born in English Canada, but have "missing" bpl in the Numident

# US birthplaces
us_bpls <- fread(path_us_bpl_codes)
us_bpls <- us_bpls[code <= 05610]
colnames(us_bpls) = c("BPLD", "usa_bpl_string")
censoc_numident_toweight <- censoc_numident_toweight %>% left_join(us_bpls, by = "BPLD")
censoc_numident_toweight[BPLD <= 05610, bpl_temp := usa_bpl_string] # us states

# unspecified US birthplaces
censoc_numident_toweight[BPLD %in% c(9000,9900), bpl_temp := "Unspecified United States"] # us states

# recode "Indian Territory" to Oklahoma
censoc_numident_toweight[BPLD == 4010, bpl_temp := "Oklahoma"]

# Current U.S. territories
censoc_numident_toweight[BPLD == 11000, bpl_temp := "Puerto Rico"]
censoc_numident_toweight[BPLD %in% c(10000:10500, 11500:12092), bpl_temp := "Other US Territory"]

# Foreign nations
# NCHS only categorizes Canada, Mexico, and Cuba. All others are aggregated.
censoc_numident_toweight[BPLD %in% 15000:15083, bpl_temp := "Canada"]
censoc_numident_toweight[BPLD == 20000, bpl_temp := "Mexico"]
censoc_numident_toweight[BPLD == 25000, bpl_temp := "Cuba"]
censoc_numident_toweight[BPLD > 15083 & BPLD != 20000 & BPLD != 25000 &
                           BPLD < 90000, bpl_temp := "Other Foreign Born"]

# Above 90000 is unspecified abroad or at sea. Only about 100 people.
censoc_numident_toweight[BPLD >= 90000, bpl_temp := "Unknown Abroad"]

# Check that these look okay
table(censoc_numident_toweight$bpl_temp, useNA = "always")


# Set these as keys and modify strings so we can match them across data sets
censoc_numident_toweight[, bpl_key := bpl_temp]
censoc_numident_toweight[, bpl_key := tolower(gsub(" ", "", bpl_key))] # remove spaces and capitals


# create flags for types of birthplaces
censoc_numident_toweight[, bpl_flag_usa := as.integer(BPLD <= 05610 & BPLD != 1500 & BPLD != 200)] # contiguous usa
censoc_numident_toweight[, bpl_flag_territory := as.integer(BPLD %in% 10000:12092)] # current territories, like P.R.
censoc_numident_toweight[, bpl_flag_AKHI := as.integer(BPLD %in% c(1500, 200))] # alaska & hawaii
censoc_numident_toweight[, bpl_flag_foreign := as.integer(BPLD > 12092 & BPLD < 90000)] # foreign nations
censoc_numident_toweight[, bpl_flag_missing :=  as.integer(bpl_temp %in% c("Unknown Abroad", "Unspecified United States"))] #missing

# make sure everything has been assigned a flag
# (this should add up to one)
mean(censoc_numident_toweight$bpl_flag_AKHI==1) + mean(censoc_numident_toweight$bpl_flag_usa==1) +
  mean(censoc_numident_toweight$bpl_flag_territory==1) + mean(censoc_numident_toweight$bpl_flag_foreign==1) +
  mean(censoc_numident_toweight$bpl_flag_missing==1)

# create keys
censoc_numident_toweight[, key := paste(paste0("a", death_age),
                                        paste0("y", dyear),
                                        paste0("s", sex_string),
                                        paste0("r", census_race),
                                        paste0("b", bpl_key),
                                        sep = "_")]

# save this dataset
write_csv(censoc_numident_toweight,
          paste0(path_out, "numident_data_prepped.csv"))


# Tabulate
censoc_numident_tab <- censoc_numident_toweight %>%
  group_by(death_age, dyear, sex_string, census_race, bpl_key,
           bpl_flag_usa, bpl_flag_territory, bpl_flag_AKHI, bpl_flag_foreign,
           bpl_flag_missing, key) %>%
  summarize(sample_n = n())

length(unique(censoc_numident_tab$key)) == nrow(censoc_numident_tab) # no repeat keys
length(unique(censoc_numident_tab$key)) == length(unique(censoc_numident_toweight$key)) # all keys captured

# save
write_csv(censoc_numident_tab,
          paste0(path_out, "numident_data_prepared_tabulated.csv"))



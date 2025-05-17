##################################################
# Last revised by Maria Osborne (Mar 21, 2023)   #
##################################################
#' Read and Harmonize NCHS mortality microdata
#'
#' This script read NCHS mortality data from individual years
#' and combines them in one file with variables recoded and
#' harmonized as appropriate.
#' 1979-2004 only


# packages
library(data.table)
library(dplyr)
library(rlist)

# Input paths
path_nchs_single_year_dat <- "/global/scratch/p2p3/pl1_demography/censoc_internal/censoc_weights/data/NCHS/nber_single_yr_data"

# Output paths
path_merged_nchs_output <- "/global/scratch/p2p3/pl1_demography/censoc_internal/censoc_weights/data/NCHS"

# list single-year files
# make sure they are listed in chronological order
files <- list.files(path_nchs_single_year_dat,
                    pattern = ".csv$", recursive = TRUE, full.names = TRUE)

# Place of birth available from 1979 - 2004 inclusive
# note: birthplace column names differ from 1979-1995, 1996-2002, 2003-2004
# the following code should be altered if files are added/removed/rearranged
# read 1979 - 1995
years_code1 <- files[5:21]
# read 1996 - 2002
years_code2 <- files[22:28]
# read 2003 - 2004
years_code3 <- files[29:30]

# read 1988 - 1995 data
nber1 <- lapply(years_code1,
                function(i) {fread(i, select = c("datayear", "sex", "age", "racer3", "statebth"))})
nber1 <- list.rbind(nber1)
# rename vars
nber1[, year := datayear]
nber1[, state_birth := statebth]
nber1$datayear <- NULL
nber1$statebth <- NULL

# read 1996 - 2002
nber2<- lapply(years_code2,
               function(i) {fread(i, select = c("year", "age", "sex", "racer3", "statbth"))})
nber2 <- list.rbind(nber2)
nber2[, state_birth := statbth]
nber2$statbth <- NULL

# read 2003 - 2004
nber3<- lapply(years_code3,
               function(i) {fread(i, select = c("year", "age", "sex", "racer3", "statbthr"))})
nber3 <- list.rbind(nber3)
nber3[, state_birth := statbthr]
nber3$statbthr <- NULL

# merge all years of data
nber_merged <- rbind(nber1, nber2, nber3)

# remove the intermediate files
rm(nber1)
rm(nber2)
rm(nber3)

# recode two digit years (79-95) to four digit (1979-1995)
nber_merged[year < 100, year := year + 1900]
table(nber_merged$year)

# deal with ages
# before 2003, ages are up to 3 digits
# 2003 onward, four digits
nber_merged[year < 2003 & age < 200, death_age_years := age]
nber_merged[year >= 2003 & age %/% 1000 == 1, death_age_years := age %% 1000]
# if death_age_years is NA, individual died before age 1

# process sex
nber_merged[sex == 1 | sex == "M", sex_string := "male"]
nber_merged[sex == 2 | sex == "F", sex_string := "female"]
table(nber_merged$sex_string)

# process race
table(nber_merged$racer3)
nber_merged[racer3 == 1, race_string := "white"]
nber_merged[racer3 == 2, race_string := "other"]
nber_merged[racer3 == 3, race_string := "black"]


# process birthplace
# 2003 onward: alphabetic codes
states_convert <- data.frame("state_name" = state.name, "state_abb" = state.abb)
nber_merged <- merge(x= nber_merged, y=states_convert, by.x="state_birth", by.y="state_abb", all.x=T)
nber_merged[, bpl := state_name]
nber_merged$state_name <- NULL
nber_merged[state_birth == "DC", bpl := "District of Columbia"]
#territories
nber_merged[state_birth == "AS", bpl := "American Samoa"]
nber_merged[state_birth == "VI", bpl := "Virgin Islands"]
nber_merged[state_birth == "GU", bpl := "Guam"]
nber_merged[state_birth == "PR", bpl := "Puerto Rico"]
nber_merged[state_birth == "MP", bpl := "Northern Marianas"]
#other nations
nber_merged[state_birth == "CC", bpl := "Canada"]
nber_merged[state_birth == "CU", bpl := "Cuba"]
nber_merged[state_birth == "MX", bpl := "Mexico"]
nber_merged[state_birth == "YY", bpl := "Other Foreign Born"]

# before 2003: numerals
# states
nber_merged[year < 2003, state_birth_numeric := as.integer(state_birth)]
nber_merged[state_birth_numeric %in% 1:8, bpl := state.name[state_birth_numeric]]
nber_merged[state_birth_numeric == 9 , bpl := "District of Columbia"]
nber_merged[state_birth_numeric %in% 10:51 , bpl := state.name[state_birth_numeric-1]]
# territories
nber_merged[state_birth_numeric == 52 , bpl := "Puerto Rico"]
nber_merged[state_birth_numeric == 53 , bpl := "Virgin Islands"]
nber_merged[state_birth_numeric == 54 , bpl := "Guam"]
nber_merged[state_birth_numeric == 61 , bpl := "American Samoa"]
nber_merged[state_birth_numeric == 62,  bpl := "Northern Marianas"]
# other nations
nber_merged[state_birth_numeric == 55 , bpl := "Canada"]
nber_merged[state_birth_numeric == 56 , bpl := "Cuba"]
nber_merged[state_birth_numeric == 57 , bpl := "Mexico"] # 58? not used.
nber_merged[state_birth_numeric == 59 , bpl := "Other Foreign Born"]

table(nber_merged$bpl) # make sure nothing here looks weird

# code birthplaces however we want
other_territories <- c("Guam", "Virgin Islands", "American Samoa", "Northern Marianas")
nber_merged[bpl %in% other_territories, bpl_key := "Other US Territory"]
nber_merged[!(bpl %in% other_territories), bpl_key := bpl]

# standardize bpl key string
nber_merged[, bpl_key := tolower(gsub(" ", "", bpl_key))] # remove spaces and capitals
table(nber_merged$bpl_key)

# create flags for types of birthplaces
cont_us <- setdiff(state.name, c("Alaska", "Hawaii"))
nber_merged[, bpl_flag_usa := as.integer(bpl %in% c(cont_us, "District of Columbia"))] # us other than AK & HI
nber_merged[, bpl_flag_territory := as.integer(bpl %in% c("Puerto Rico", other_territories))]
nber_merged[, bpl_flag_AKHI := as.integer(bpl %in% c("Alaska", "Hawaii"))]
nber_merged[, bpl_flag_foreign := as.integer(bpl %in% c("Canada", "Cuba", "Mexico", "Other Foreign Born"))]
nber_merged[, bpl_flag_missing := as.integer(is.na(bpl))]

# make sure these add up to 1
mean(nber_merged$bpl_flag_AKHI==1) + mean(nber_merged$bpl_flag_usa==1) +
  mean(nber_merged$bpl_flag_territory==1) + mean(nber_merged$bpl_flag_foreign==1) +
  mean(nber_merged$bpl_flag_missing==1)

# create keys
nber_merged[, key := paste(paste0("a", death_age_years),
                           paste0("y", year),
                           paste0("s", sex_string),
                           paste0("r", race_string),
                           paste0("b", bpl_key),
                           sep = "_")]


# save data set
write.csv(nber_merged, paste0(path_merged_nchs_output, "/NCHS_data_harmonized.csv"),
          row.names = FALSE)

# note: this dataset contains all death ages, all birthplaces
# further filtering must be done later

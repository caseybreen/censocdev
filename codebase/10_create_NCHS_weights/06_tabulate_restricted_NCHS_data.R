#####################################################
# Last revised by Maria Osborne (August 15, 2023)   #
#####################################################
#'
#' Create tabulations for NCHS 2005 Data
#'

# Libraries
library(data.table)
library(dplyr)
library(readr)

# Input path
path_NCHS_harmonized_data <- "/global/home/users/mariaosborne/CenSoc Weights/data/nchs_restricted/NCHS_data_merged_2005_to_2020.csv"

# Output paths
path_merged_nchs_output <- "/global/home/users/mariaosborne/CenSoc Weights/data/nchs_restricted/"

# read in data
nchs_data <- fread(path_NCHS_harmonized_data)

# restrict to relevant ages
nchs_data <- nchs_data[death_age_years %in% 65:100] # & !is.na(bpl)]

# select relevant variables
nchs_data <- nchs_data[, .(death_age_years, year, sex_string, race_string, bpl, bpl_key,
                           bpl_flag_usa, bpl_flag_territory, bpl_flag_AKHI,
                           bpl_flag_foreign, bpl_flag_missing, key)]

# tabulate population
population_tabulated <- nchs_data[, population_n := .N, by = .(key)][order(key)]
# take unique so we have one row per stratum instead of per person
population_tabulated <- unique(population_tabulated, by = "key")

# make sure there are no duplicate keys
length(unique(population_tabulated$key))== nrow(population_tabulated)

# save tabulated data
write_csv(population_tabulated,
          paste0(path_merged_nchs_output, "/NCHS_data_harmonized_tabulated_2005_to_2020.csv"))



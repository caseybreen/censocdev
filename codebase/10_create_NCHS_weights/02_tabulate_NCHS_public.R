#####################################################
# Last revised by Maria Osborne (August 11, 2023)   #
#####################################################
#'
#' Create tabulations for NCHS Data Prior to 2005
#'

# Libraries
library(data.table)
library(dplyr)
library(readr)

# Input path
path_NCHS_harmonized_data <- "/global/scratch/p2p3/pl1_demography/censoc_internal/censoc_weights/data/NCHS/NCHS_data_harmonized.csv"

# Output paths
path_merged_nchs_output <- "/global/scratch/p2p3/pl1_demography/censoc_internal/censoc_weights/data/NCHS"

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


# note: need to make sure that there are no duplicate keys from when bpl has multiple values
# that can correspond to the same bpl_key

write_csv(population_tabulated,
          paste0(path_merged_nchs_output, "/NCHS_public_data_harmonized_tabulated.csv"))



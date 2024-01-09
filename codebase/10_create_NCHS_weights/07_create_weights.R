####################################################
# Last revised by Maria Osborne (August __ 2023)   #
####################################################
#' 
#' Calls weighting functions:
#' create_NCHS_weights_numident()
#' create_NCHS_weights_dmf()
#' 
#' Input: processed and tabulated CenSoc Numident and/or DMF data set,
#' tabulated NCHS mortality data
#' Output: weights by strata 
#' 
#' Assumes years 1975-2005 for DMF and 1988-2005 for Numident.
#' If running on public server where 2005+ NCHS data are not available,
#' user will have to change the end_year argument to functions
#' 
#'

# libraries
library(data.table)
library(tidyverse)
library(sampling) # will use calibration function from here
library(caret)
library(MASS)

# Source master functions
source("~/CenSoc Weights/code/master_weighting_function.R")

# Paths
numident_tabulated_path <- "~/CenSoc Weights/data/numident/numident_data_prepared_tabulated.csv"
dmf_tabulated_path <- "~/CenSoc Weights/data/dmf/dmf_data_prepared_tabulated.csv"
path_population_data_public <- "~/CenSoc Weights/data/nchs_public/NCHS_public_data_harmonized_tabulated.csv"
path_population_data_restricted <- "~/CenSoc Weights/data/nchs_restricted/NCHS_data_harmonized_tabulated_2005_to_2020.csv"

# Read tabulated NCHS population death data for ages 65-100
population_data_pre_2005 <- fread(path_population_data_public)
population_data_later <- fread(path_population_data_restricted)
population_data <- rbind(population_data_pre_2005, population_data_later[year == 2005])
rm(population_data_pre_2005)
rm(population_data_later)

# Read tabulated CenSoc files
tabulated_numident <- fread(numident_tabulated_path)
tabulated_numident <- tabulated_numident[dyear %in% 1988:2005]
tabulated_dmf <- fread(dmf_tabulated_path)
tabulated_dmf <- tabulated_dmf[dyear %in% 1975:2005]

# Compute Numident weights (will take several minutes due to raking step)
weights_numident <- create_NCHS_weights_numident(censoc_data = tabulated_numident,
                                                 tabulated_population_data = population_data,
                                                 verbose = TRUE, debug = TRUE)

# Compute DMF weights (will take several minutes due to raking steps)
weights_dmf <- create_NCHS_weights_dmf(censoc_data = tabulated_dmf,
                                       tabulated_population_data = population_data,
                                       verbose = TRUE, debug = TRUE)

write_csv(weights_numident, "~/CenSoc Weights/data/numident/numident_weights_by_strata.csv")
write_csv(weights_dmf, "~/CenSoc Weights/data/dmf/dmf_weights_by_strata.csv")







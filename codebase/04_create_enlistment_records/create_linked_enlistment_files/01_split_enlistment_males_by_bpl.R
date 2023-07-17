# Split WWII Enlistment File by birthplace
# Author: Maria Osborne
# Updated: June 17 2023

# source split function
source("~/censocdev/R/split-by-bpl.R")

# Library packages, set paths
library(data.table)
library(readr)
library(dplyr)

# paths
in_path_to_split <- "/data/censoc/workspace/enlistment_records/finalenlistment_for_linking.csv"
out_path_split <- "~mariaosborne-ipums/enlistment_records_linking/enlistment-males-by-bpl/"
split_var <- "bpl"
cols_to_keep <- c("fname", "lname", "sex", "byear", "bpl", "unique_ID")

# read in full enlistment file
enlistment_full <- fread(in_path_to_split)

# give each row a unique identifier (as serial numbers are not always unique)
enlistment_full <- enlistment_full %>% mutate(unique_ID = row_number())

# restrict to columns needed for matching
data_small <- enlistment_full[ , ..cols_to_keep]

rm(enlistment_full)

# Split dataset on bpld_r
dir.create(out_path_split)
setwd(out_path_split)

# restrict to men
data_small <- data_small[sex==1]

# replace NA birthplace with 99999
data_small$bpl[is.na(data_small$bpl)] <- 99999

# get rid of people missing a matching field (name or birthyear)
data_small <- data_small[!is.na(fname) & !is.na(lname) & !is.na(byear)]

# Run split by bpl code
# Returns list where where each element is a bpl
data_split <- split(data_small, data_small[[split_var]])

# write to file system
for(bpl in data_split) {
  temp1 <- as.data.frame(bpl)
  temp2 <- paste0("bpl_", as.character(temp1[[split_var]][1]))
  write_csv(temp1, paste0(out_path_split, temp2, ".csv"))
  rm(temp1, temp2)
}

# clean up
rm(list = ls())

# Match enlistment males to social security DMF
# Author: Maria Osborne, after Wok-tak Joo and Uger Yildirim
# Updated: July 2, 2023

# Library packages, source functions, set paths
library(dplyr)
library(readr)
library(stringi)
library(stringr)
library(tidyverse)
library(data.table)
library(here)

# source for matching code
source(here("R/match-records.R"))

# File B: DMF (whole DMF, not the matched censoc-DMF, N = 54.6 million, sex=1 only)
in_path_dmf <- "/home/ipums/wontak.joo-ipums/project/abe/data/cleaned-dmf/dmf_cleaned.csv"
# file_B: Enlistment (N = 9 million)
in_path_clean_enlistment <-"/global/scratch/p2p3/pl1_demography/censoc/workspace/enlistment_records/finalenlistment_for_linking.csv"
# Out path:
out_path <- "/global/scratch/p2p3/pl1_demography/censoc_internal/enlistment_records_linking/matched_enlistment_dmf/"

dmf <- fread(in_path_dmf)
enlistment <- fread(in_path_clean_enlistment)

# prepare enlistment files for linking by assigning unique ID, limit to men.
enlistment <- enlistment %>% mutate(unique_ID = row_number())
enlistment <- enlistment %>% filter(sex==1) # only attempt to match men

#  Match enlistment records to CenSoc-DMF records
#  A = DMF, B = enlistment
file_A_male <- dmf
file_B_male <- enlistment
fname_var_A    <- "fname"
fname_var_B    <- "fname"
lname_var_A    <- "lname"
lname_var_B    <- "lname"
time_var_A     <- "byear"
time_var_B     <- "byear"
id_var_A       <- "ssn"
id_var_B       <- "unique_ID"
vars_to_keep_A <- c("fname_raw", "lname_raw", "middle", "middle_raw", "sex",
                    "bmonth", "dyear", "dmonth", "death_age", "weight")
vars_to_keep_B <- c("mname")

# Drop if names are missing
file_A_male <- file_A_male[file_A_male[[fname_var_A]]!="" & file_A_male[[lname_var_A]]!=""]
file_B_male <- file_B_male[file_B_male[[fname_var_B]]!="" & file_B_male[[lname_var_B]]!=""]
file_B_male <- file_B_male[!is.na(fname) & !is.na(lname)]

# perform matching
out_file_name <- "matched_enlistment_dmf.csv"
match_records(file_A_male, fname_var_A, lname_var_A, time_var_A, id_var_A, vars_to_keep_A,
              file_B_male, fname_var_B, lname_var_B, time_var_B, id_var_B, vars_to_keep_B,
              out_path, out_file_name)

#2,897,630 rows

# clean up
rm(list = ls())



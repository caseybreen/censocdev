#########################################
# created by UGUR YILDIRIM (2020-08-17) #
# revised by WON-TAK JOO (2022-11-10)   #
#########################################

# Source functions
library(here)
source(here("R/clean-names.R"))

# Load libraries
library(stringi)
library(tidyverse)
library(data.table)

# Paths
tan_path          <- here("codebase/02_create_censoc/titles-and-nicknames")
in_path_census    <- "/global/scratch/p2p3/pl1_demography/censoc_internal/abe/data/1940-census-by-bpl"
out_path_census   <- "/global/scratch/p2p3/pl1_demography/censoc_internal/abe/data/cleaned-1940-census-by-bpl"
in_path_bunmd     <- "/global/scratch/p2p3/pl1_demography/censoc_internal/abe/data/bunmd-by-bpl"
out_path_bunmd    <- "/global/scratch/p2p3/pl1_demography/censoc_internal/abe/data/2.1_data/cleaned-bunmd-by-bpl"
in_path_dmf       <- "/global/scratch/p2p3/pl1_demography/censoc_internal/abe/data/dmf"
out_path_dmf      <- "/global/scratch/p2p3/pl1_demography/censoc_internal/abe/data/2.1_data/cleaned-dmf"
log_path          <- "/global/scratch/p2p3/pl1_demography/censoc_internal/abe/log"

path_to_out_file <- paste0(log_path, "/3-run-clean-names.log")
log <- file(path_to_out_file, open="wt")
sink(log, type="message")

# Read titles and nicknames
titles <- fread(paste(tan_path, "titles.csv", sep="/"))
male_nicknames <- fread(paste(tan_path, "male_nicknames.csv", sep="/"))
female_nicknames <- fread(paste(tan_path, "female_nicknames.csv", sep="/"))
nicknames <- data.table(rbind(male_nicknames, female_nicknames))
old_nicknames <- fread(paste(tan_path, "old_nicknames.csv", sep="/"))

# Clean census names
lname_col_census <- "NAMELAST"
fname_col_census <- "NAMEFRST"
sex_col_census <- "SEX"
other_cols_census <- c("BIRTHYR", "BPL", "SERIALP", "PERNUM", "HISTID", "MARST", "AGE", "RACE")
middle_census <- ""
#file_name_census <- "bpl_100.csv" # JUST AS AN EXAMPLE
#clean_names(in_path_census, file_name_census, lname_col_census, fname_col_census,
#sex_col_census, other_cols_census, out_path_census, middle_census)
for (file_name in list.files(in_path_census)) {
  cat("Processing... ", file_name)
  clean_names(in_path_census, file_name, lname_col_census, fname_col_census,
              sex_col_census, other_cols_census, out_path_census, middle_census)
}

# Clean BUNMD names
lname_col_bunmd <- "lname"
fname_col_bunmd <- "fname"
sex_col_bunmd <- "sex"
other_cols_bunmd <- c("byear", "bmonth", "dyear", "dmonth", "death_age", "census_age",
                      "race_first", "race_first_cyear", "race_first_cmonth",
                      "race_last", "race_last_cyear", "race_last_cmonth",
                      "bpl", "zip_residence", "socstate", "age_first_application", "weight",
                      "ssn", "father_lname")
middle_bunmd <- "mname"
#file_name_bunmd <- "bpl_100.csv" # JUST AS AN EXAMPLE
#clean_names(in_path_bunmd, file_name_bunmd, lname_col_bunmd, fname_col_bunmd,
#sex_col_bunmd, other_cols_bunmd, out_path_bunmd, middle_bunmd)
for (file_name in list.files(in_path_bunmd)) {
  cat("Processing... ", file_name)
  clean_names(in_path_bunmd, file_name, lname_col_bunmd, fname_col_bunmd,
              sex_col_bunmd, other_cols_bunmd, out_path_bunmd, middle_bunmd)
}

# Clean DMF names
lname_col_dmf <- "lname"
fname_col_dmf <- "fname"
sex_col_dmf <- "sex"
other_cols_dmf <- c("byear", "bmonth", "dyear", "dmonth", "death_age", "census_age", "weight", "ssn")
middle_dmf <- "mname"
file_name_dmf <- "dmf.csv"
clean_names(in_path_dmf, file_name_dmf, lname_col_dmf, fname_col_dmf,
            sex_col_dmf, other_cols_dmf, out_path_dmf, middle_dmf)

# Clean up after yourself
sink(type="message")
close(log)
rm(list = ls())

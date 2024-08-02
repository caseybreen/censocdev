# Match enlistment males to BUNMD
# Author: Maria Osborne, after Wok-tak Joo and Uger Yildirim
# Updated: July 26, 2024

# Library packages, source functions, set paths
library(dplyr)
library(readr)
library(stringi)
library(stringr)
library(tidyverse)

# source for matching code
source("~/censocdev/R/match-records.R")

# file_A: BUNMD by bpl
in_path_bunmd <- "~mariaosborne-ipums/cleaned-bunmd-by-bpl"

# file_B: Enlistment by bpl
in_path_enlistment <- "~mariaosborne-ipums/enlistment_records_linking/enlistment-males-by-bpl-v1.1"

# Out path:
out_path <- "/home/ipums/mariaosborne-ipums/enlistment_records_linking/matched-enlistment-numident-males-v1.1"

# Prepare file names for matching
bunmd_files <- list.files(in_path_bunmd)
enlistment_files_males <- list.files(in_path_enlistment)

intersection         <- intersect(str_replace(bunmd_files, "_cleaned", ""), enlistment_files_males)
# 125 bpls in both
bunmd_only <- bunmd_files[!str_replace(bunmd_files, "_cleaned", "") %in% intersection]
# 187 only in bunmd (all non-US)
enlistment_only      <- enlistment_files_males[!enlistment_files_males %in% intersection]
# 10 only in enlistment

#  Match enlistment records to CenSoc-Numident records
#  A = BUNMD, B = enlistment
fname_var_A    <- "fname"
fname_var_B    <- "fname"
lname_var_A    <- "lname"
lname_var_B    <- "lname"
time_var_A     <- "byear"
time_var_B     <- "byear"
id_var_A       <- "ssn"
id_var_B       <- "unique_ID"
vars_to_keep_A <-  c("fname_raw", "lname_raw", "middle", "middle_raw", "sex", "bpl",
                     "bmonth", "dyear", "dmonth", "death_age",
                     "race_first", "race_first_cyear", "race_first_cmonth",
                     "race_last", "race_last_cyear", "race_last_cmonth",
                     "zip_residence", "socstate", "age_first_application", "weight")
vars_to_keep_B <- c("bpl")

error_male <- c()

dir.create(out_path)
# Perform matching (will take a few minutes)
for (file_name in intersection) {
  file_name_bunmd <- str_replace(file_name, ".csv", "_cleaned.csv")

  # read files
  file_A  <- fread(paste(in_path_bunmd, file_name_bunmd, sep="/"))
  file_A_male <- file_A[sex==1]
  file_B_male <- fread(paste(in_path_enlistment, file_name, sep="/"))

  # Drop if names are missing
  file_A_male <- file_A_male[file_A_male[[fname_var_A]]!="" & file_A_male[[lname_var_A]]!=""]
  file_B_male <- file_B_male[file_B_male[[fname_var_B]]!="" & file_B_male[[lname_var_B]]!=""]

  # try matching
  if (nrow(file_A_male) > 0 & nrow(file_B_male) > 0) {
    out_file_name <- str_replace(file_name, ".csv", "_matched_male.csv")
    res <- try(match_records(file_A_male, fname_var_A, lname_var_A, time_var_A, id_var_A, vars_to_keep_A,
                             file_B_male, fname_var_B, lname_var_B, time_var_B, id_var_B, vars_to_keep_B,
                             out_path, out_file_name), silent=TRUE)
    if (class(res) == "try-error") {
      error_male <- c(error_male, file_name)
    }
  }

}

# check for errors
error_male # no matches within these birthplaces


# Merge match files, save
match_files <- list.files(out_path)

setwd(out_path)

tables_match <- lapply(match_files, function(i){
  read_csv(i, col_types = cols(
    id_A             = col_character(), #ssn
    id_B             = col_integer(), # enlistment uniqu_id
    fname            = col_character(),
    lname            = col_character(),
    byear_B         = col_integer(),
    byear_A    = col_integer(),
    timediff_A       = col_integer(),
    timediff_B       = col_integer(),
    bpl_B            = col_integer(),
    bpl_census_A     = col_integer(),
    uniquestub_match1 = col_integer(),
    uniquestub_file1  = col_integer(),
    uniquestub_match2 = col_integer(),
    uniquestub_file2  = col_integer()
  ))
})

matched_numident_enlistment_male <- do.call(rbind, tables_match) # 2.75 million observations

write_csv(matched_numident_enlistment_male,
          paste0(out_path, "/matched_numident_enlistment_male.csv"))

# clean up
rm(list = ls())

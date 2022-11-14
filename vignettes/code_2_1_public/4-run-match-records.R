#########################################
# created by UGUR YILDIRIM (2020-09-29) #
# revised by WON-TAK JOO (2022-11-10)   #
#########################################

# Paths & source codes should be set up for each matching process for BUMD & DMF

# Load libraries
library(data.table)
library(dplyr)
library(readr)
library(stringi)
library(stringr)
library(tidyverse)

### Match census to BUNMD
in_path_A      <- "/abe/data/cleaned-1940-census-by-bpl"
in_path_B      <- "/abe/data/cleaned-bunmd-by-bpl"
fname_var_A    <- "fname"
fname_var_B    <- "fname"
lname_var_A    <- "lname"
lname_var_B    <- "lname"
time_var_A     <- "AGE"
time_var_B     <- "census_age"
id_var_A       <- "HISTID"
id_var_B       <- "ssn"
vars_to_keep_A <- c("fname_raw", "lname_raw", "middle", "sex", "BIRTHYR", "BPL", "SERIALP", "PERNUM", "MARST", "RACE")
vars_to_keep_B <- c("fname_raw", "lname_raw", "middle", "middle_raw", "sex", "bpl",
                    "byear", "bmonth", "dyear", "dmonth", "death_age",
                    "race_first", "race_first_cyear", "race_first_cmonth",
                    "race_last", "race_last_cyear", "race_last_cmonth",
                    "zip_residence", "socstate", "age_first_application", "weight",
                    "father_lname_raw", "father_lname")
out_path       <- "~/project/abe/data/matched-census-bunmd"
log_path       <- "~/project/abe/log/"

path_to_out_file <- paste0(log_path, "/4-run-match-records_1.log")
log <- file(path_to_out_file, open="wt")
sink(log, type="message")

source("/censocdev/R/clean-names.R")
source("/censocdev/R/match-records.R")

#file_name_A    <- "bpl_1000_cleaned.csv" # JUST AS AN EXAMPLE
#file_name_B    <- "bpl_1000_cleaned.csv" # JUST AS AN EXAMPLE
#out_file_name  <- "bpl_1000_matched.csv" # JUST AS AN EXAMPLE
#file_A <- read_csv(paste(in_path_A, file_name_A, sep="/"))
#file_B <- read_csv(paste(in_path_B, file_name_B, sep="/"),
#col_types = cols(middle = col_character(), middle_raw = col_character()))
#file_B$father_lname_raw <- file_B$father_lname
#file_B$father_lname <- ""
#match_records(file_A, fname_var_A, lname_var_A, time_var_A, id_var_A, vars_to_keep_A,
#file_B, fname_var_B, lname_var_B, time_var_B, id_var_B, vars_to_keep_B,
#out_path, out_file_name)

dir.create(out_path)

census_files <- list.files(in_path_A)
bunmd_files <- list.files(in_path_B)
intersection <- intersect(census_files, bunmd_files)
census_only <- census_files[!census_files %in% intersection]
bunmd_only <- bunmd_files[!bunmd_files %in% intersection]

error_male          <- c()
error_female_everm  <- c()
error_female_neverm <- c()

for (file_name in intersection) {
  
  file_A <- fread(paste(in_path_A, file_name, sep="/"))
  file_B <- fread(paste(in_path_B, file_name, sep="/"))
  file_B$father_lname_raw <- file_B$father_lname
  file_B$father_lname <- ""
  
  # Drop if names are missing
  file_A <- file_A[file_A[[fname_var_A]]!="" & file_A[[lname_var_A]]!=""]
  file_B <- file_B[file_B[[fname_var_B]]!="" & file_B[[lname_var_B]]!=""]

  # Process males
  file_A_male <- subset(file_A, sex == 1)
  file_B_male <- subset(file_B, sex == 1)
  
  if (nrow(file_A_male) > 0 & nrow(file_B_male) > 0) {
    out_file_name <- gsub("_cleaned", "_matched_male", file_name)
    res <- try(match_records(file_A_male, fname_var_A, lname_var_A, time_var_A, id_var_A, vars_to_keep_A,
                             file_B_male, fname_var_B, lname_var_B, time_var_B, id_var_B, vars_to_keep_B,
                             out_path, out_file_name), silent=TRUE)
    if (class(res) == "try-error") {
      error_male <- c(error_male, file_name)
    }
  }
  
  # Process females
  file_A_female <- subset(file_A, sex == 2)
  file_B_female <- subset(file_B, sex == 2)
  
  # Ever-married females
  file_A_female_everm <- subset(file_A_female, MARST < 6)
  if (nrow(file_A_female_everm) > 0 & nrow(file_B_female) > 0) {
    out_file_name <- gsub("_cleaned", "_matched_female_everm", file_name)
    res <- try(match_records(file_A_female_everm, fname_var_A, lname_var_A, time_var_A, id_var_A, vars_to_keep_A,
                             file_B_female,       fname_var_B, lname_var_B, time_var_B, id_var_B, vars_to_keep_B,
                             out_path, out_file_name), silent=TRUE)
    if (class(res) != "try-error") {
      matched_female_everm <- read_csv(paste(out_path, out_file_name, sep="/"), 
                                       col_types = cols(timediff_B = col_double(),
                                                        middle_A = col_character(),
                                                        middle_B = col_character(),
                                                        middle_raw_B = col_character()))
      ids <- matched_female_everm$id_B
    } else {
      error_female_everm <- c(error_female_everm, file_name)
      ids <- c()
    }
  }
  
  # Never-married females
  file_A_female_neverm <- subset(file_A_female, MARST == 6)
  file_B_female_unmatched <- subset(file_B_female, !ssn %in% ids)
  if (nrow(file_A_female_neverm) > 0 & nrow(file_B_female_unmatched) > 0) {
    # Clean father_lname
    file_B_female_unmatched$father_lname <- file_B_female_unmatched$father_lname_raw
    file_B_female_unmatched$father_lname <- iconv(file_B_female_unmatched$father_lname, to="ASCII//TRANSLIT")
    file_B_female_unmatched$father_lname <- tolower(file_B_female_unmatched$father_lname)
    file_B_female_unmatched$father_lname <- str_trim(file_B_female_unmatched$father_lname)
    file_B_female_unmatched$father_lname <- gsub("\\.", " ", file_B_female_unmatched$father_lname)
    file_B_female_unmatched$father_lname <- gsub("[^a-z ]", "", file_B_female_unmatched$father_lname)
    file_B_female_unmatched$father_lname <- paste0(" ", file_B_female_unmatched$father_lname)
    file_B_female_unmatched$father_lname <- gsub(" st ",    " st",    file_B_female_unmatched$father_lname)
    file_B_female_unmatched$father_lname <- gsub(" ste ",   " ste",   file_B_female_unmatched$father_lname)
    file_B_female_unmatched$father_lname <- gsub(" saint ", " saint", file_B_female_unmatched$father_lname)
    file_B_female_unmatched$father_lname <- gsub(" virg ",  " virg",  file_B_female_unmatched$father_lname)
    file_B_female_unmatched$father_lname <- gsub(" mac ",   " mac",   file_B_female_unmatched$father_lname)
    file_B_female_unmatched$father_lname <- gsub(" mc ",    " mc",    file_B_female_unmatched$father_lname)
    file_B_female_unmatched$father_lname <- str_trim(file_B_female_unmatched$father_lname)
    res <- lapply(file_B_female_unmatched$father_lname, split_first_name)
    file_B_female_unmatched$father_lname <- unlist(res)[attr(unlist(res),"names") == "first"]
    rm(res)
    file_B_female_unmatched$father_lname <- str_trim(file_B_female_unmatched$father_lname)
    # Match
    out_file_name <- gsub("_cleaned", "_matched_female_neverm", file_name)
    res <- try(match_records(file_A_female_neverm,    fname_var_A, lname_var_A,    time_var_A, id_var_A, vars_to_keep_A,
                             file_B_female_unmatched, fname_var_B, "father_lname", time_var_B, id_var_B, vars_to_keep_B,
                             out_path, out_file_name), silent=TRUE)
    if (class(res) == "try-error") {
      error_female_neverm <- c(error_female_neverm, file_name)
    }
  }
  
  # NOTES
  # 1. When matching females, maybe we shouldn't pre-split ever/never married.
  #    Maybe what's better is to first try to match all females using their
  #    last name, and then try to match the remaining using father's last name.
  
}

# Now, match the remaining records in Census and BUNMD

setwd(in_path_A)
tables_A <- lapply(census_only, function(i){
  fread(i)
})
combined_A <- do.call(rbind, tables_A)

setwd(in_path_B)
tables_B <- lapply(bunmd_only, function(i){
  fread(i)
})
combined_B <- do.call(rbind, tables_B)

combined_B$father_lname_raw <- combined_B$father_lname
combined_B$father_lname <- ""

combined_A <- combined_A[combined_A[[fname_var_A]]!="" & combined_A[[lname_var_A]]!=""]
combined_B <- combined_B[combined_B[[fname_var_B]]!="" & combined_B[[lname_var_B]]!=""]

file_A_male <- subset(combined_A, sex == 1)
file_B_male <- subset(combined_B, sex == 1)

out_file_name <- "rest_matched_male.csv"
match_records(file_A_male, fname_var_A, lname_var_A, time_var_A, id_var_A, vars_to_keep_A,
              file_B_male, fname_var_B, lname_var_B, time_var_B, id_var_B, vars_to_keep_B,
              out_path, out_file_name)

file_A_female <- subset(combined_A, sex == 2)
file_B_female <- subset(combined_B, sex == 2)

file_A_female_everm <- subset(file_A_female, MARST < 6)
out_file_name <- "rest_matched_female_everm.csv"
match_records(file_A_female_everm, fname_var_A, lname_var_A, time_var_A, id_var_A, vars_to_keep_A,
              file_B_female,       fname_var_B, lname_var_B, time_var_B, id_var_B, vars_to_keep_B,
              out_path, out_file_name)
matched_female_everm <- read_csv(paste(out_path, out_file_name, sep="/"), 
                                 col_types = cols(timediff_B = col_double(),
                                                  middle_A = col_character(),
                                                  middle_B = col_character(),
                                                  middle_raw_B = col_character()))
ids <- matched_female_everm$id_B

file_A_female_neverm <- subset(file_A_female, MARST == 6)
file_B_female_unmatched <- subset(file_B_female, !ssn %in% ids)
file_B_female_unmatched$father_lname <- file_B_female_unmatched$father_lname_raw
file_B_female_unmatched$father_lname <- iconv(file_B_female_unmatched$father_lname, to="ASCII//TRANSLIT")
file_B_female_unmatched$father_lname <- tolower(file_B_female_unmatched$father_lname)
file_B_female_unmatched$father_lname <- str_trim(file_B_female_unmatched$father_lname)
file_B_female_unmatched$father_lname <- gsub("\\.", " ", file_B_female_unmatched$father_lname)
file_B_female_unmatched$father_lname <- gsub("[^a-z ]", "", file_B_female_unmatched$father_lname)
file_B_female_unmatched$father_lname <- paste0(" ", file_B_female_unmatched$father_lname)
file_B_female_unmatched$father_lname <- gsub(" st ",    " st",    file_B_female_unmatched$father_lname)
file_B_female_unmatched$father_lname <- gsub(" ste ",   " ste",   file_B_female_unmatched$father_lname)
file_B_female_unmatched$father_lname <- gsub(" saint ", " saint", file_B_female_unmatched$father_lname)
file_B_female_unmatched$father_lname <- gsub(" virg ",  " virg",  file_B_female_unmatched$father_lname)
file_B_female_unmatched$father_lname <- gsub(" mac ",   " mac",   file_B_female_unmatched$father_lname)
file_B_female_unmatched$father_lname <- gsub(" mc ",    " mc",    file_B_female_unmatched$father_lname)
file_B_female_unmatched$father_lname <- str_trim(file_B_female_unmatched$father_lname)
res <- lapply(file_B_female_unmatched$father_lname, split_first_name)
file_B_female_unmatched$father_lname <- unlist(res)[attr(unlist(res),"names") == "first"]
rm(res)
file_B_female_unmatched$father_lname <- str_trim(file_B_female_unmatched$father_lname)
out_file_name <- "rest_matched_female_neverm.csv"
match_records(file_A_female_neverm,    fname_var_A, lname_var_A,    time_var_A, id_var_A, vars_to_keep_A,
              file_B_female_unmatched, fname_var_B, "father_lname", time_var_B, id_var_B, vars_to_keep_B,
              out_path, out_file_name)

match_files <- list.files(out_path)
setwd(out_path)
tables_match <- lapply(match_files, function(i){
  fread(i)
})
matched_census_bunmd <- do.call(rbind, tables_match)
matched_census_bunmd$flag_bpl_NA_in_B   <- ifelse(matched_census_bunmd$bpl_B == 99999, 1, 0)
matched_census_bunmd$flag_bpl_only_in_A <- ifelse(matched_census_bunmd$BPL_A %in% as.integer(str_extract(census_only, "[0-9]+")), 1, 0)
matched_census_bunmd$flag_bpl_only_in_B <- ifelse(matched_census_bunmd$bpl_B %in% as.integer(str_extract(bunmd_only, "[0-9]+")), 1, 0)
fwrite(matched_census_bunmd, "matched_census_bunmd.csv")

# Clean up after yourself
sink(type="message")
close(log)
rm(list = ls())

### Match census to DMF
in_path_A      <- "/abe/data/cleaned-1940-census-by-bpl"
in_path_B      <- "/abe/data/cleaned-dmf"
fname_var_A    <- "fname"
fname_var_B    <- "fname"
lname_var_A    <- "lname"
lname_var_B    <- "lname"
time_var_A     <- "AGE"
time_var_B     <- "census_age"
id_var_A       <- "HISTID"
id_var_B       <- "ssn"
vars_to_keep_A <- c("fname_raw", "lname_raw", "middle", "sex", "BIRTHYR", "BPL", "SERIALP", "PERNUM", "MARST", "RACE")
vars_to_keep_B <- c("fname_raw", "lname_raw", "middle", "middle_raw", "sex",
                    "byear", "bmonth", "dyear", "dmonth", "death_age", "weight")
out_path       <- "/abe/data/matched-census-dmf"
log_path       <- "/abe/log/"

path_to_out_file <- paste0(log_path, "/4-run-match-records_2.log")
log <- file(path_to_out_file, open="wt")
sink(log, type="message")

source("/censocdev/R/clean-names.R")
source("/censocdev/R/match-records.R")

dir.create(out_path)

census_files <- list.files(in_path_A)
setwd(in_path_A)
tables_census <- lapply(census_files, function(i){
  fread(i)
})
census <- do.call(rbind, tables_census)
census_male <- subset(census, sex == 1)
rm(census, tables_census)

dmf <- fread(paste(in_path_B, "dmf_cleaned.csv", sep="/"))

out_file_name <- "matched_census_dmf.csv"
match_records(census_male, fname_var_A, lname_var_A, time_var_A, id_var_A, vars_to_keep_A,
              dmf,         fname_var_B, lname_var_B, time_var_B, id_var_B, vars_to_keep_B,
              out_path, out_file_name)

# Clean up after yourself
sink(type="message")
close(log)
rm(list = ls())
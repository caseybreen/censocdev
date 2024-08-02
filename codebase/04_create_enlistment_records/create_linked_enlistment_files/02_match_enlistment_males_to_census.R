# Match enlistment records to census
# Author: Maria Osborne, after Wok-tak Joo and Uger Yildirim
# Updated: July 26, 2024

# Load libraries
library(data.table)
library(dplyr)
library(readr)
library(stringi)
library(stringr)
library(tidyverse)

# source for matching function
source("~/censocdev/R/match-records.R")

### Match census to enlistment
# File A = census
# File B = enlistment
in_path_A      <- "/home/ipums/wontak.joo-ipums/project/abe/data/cleaned-1940-census-by-bpl/" # note: files contain both sexes
in_path_B      <- "/home/ipums/mariaosborne-ipums/enlistment_records_linking/enlistment-males-by-bpl-v1.1/"
fname_var_A    <- "fname"
fname_var_B    <- "fname"
lname_var_A    <- "lname"
lname_var_B    <- "lname"
time_var_A     <- "BIRTHYR" # changed from AGE
time_var_B     <- "byear"
# notes: enlistment doesn't have birth month or birth day
# thus we can't calculate age at time of census.
# so we'll just use byear and the IPUMS census BIRTHYR var
# even though BIRTHYR can be off by one year for a lot of people
id_var_A       <- "HISTID"
id_var_B       <- "unique_ID"
vars_to_keep_A <- c("fname_raw", "lname_raw", "middle", "sex", "AGE", "BPL", "SERIALP", "PERNUM", "MARST", "RACE")
vars_to_keep_B <- c("bpl")
out_path       <- "/home/ipums/mariaosborne-ipums/enlistment_records_linking/matched-enlistment-census-v1.1/"
log_path       <- "/home/ipums/mariaosborne-ipums/enlistment_records_linking/log"

path_to_out_file <- paste0(log_path, "/run-match-records_2.log")
log <- file(path_to_out_file, open="wt")
sink(log, type="message")


# prepare files for linking
census_files <- list.files(in_path_A)
enlistment_files <- list.files(in_path_B)
intersection <- intersect(str_remove(census_files, "_cleaned"), enlistment_files)
census_only <- census_files[!str_remove(census_files, "_cleaned") %in% intersection]
enlistment_only <- enlistment_files[!enlistment_files %in% intersection]

error_male <- c()


# perform linking
for (file_name in intersection) {

  # file A: census file, needs the "_cleaned" suffix
  file_A <- fread(paste(in_path_A,
                        str_replace(file_name, ".csv", "_cleaned.csv"),
                        sep="/"))
  # file B: enlistment, no "cleaned" suffix on file names
  file_B <- fread(paste(in_path_B, file_name, sep="/"))

  # Drop if names are missing
  file_A <- file_A[file_A[[fname_var_A]]!="" & file_A[[lname_var_A]]!=""]
  file_B <- file_B[file_B[[fname_var_B]]!="" & file_B[[lname_var_B]]!=""]

  # Process males
  file_A_male <- subset(file_A, sex == 1)
  file_B_male <- subset(file_B, sex == 1)

  if (nrow(file_A_male) > 0 & nrow(file_B_male) > 0) {
    out_file_name <- gsub(".csv", "_matched_male.csv", file_name)
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
    id_A             = col_character(),
    id_B             = col_integer(),
    fname            = col_character(),
    lname            = col_character(),
    byear_B         = col_integer(),
    byear_bunmd_A    = col_integer(),
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

matched_census_enlistment_male <- do.call(rbind, tables_match) # 3.44 million rows

write_csv(matched_census_enlistment_male, "matched_census_enlistment_male.csv")

# clean up
rm(list = ls())


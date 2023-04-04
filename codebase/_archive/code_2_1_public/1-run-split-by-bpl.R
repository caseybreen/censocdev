#########################################
# created by UGUR YILDIRIM (2020-08-17) #
# revised by WON-TAK JOO (2022-11-10)   #
#########################################

# Source functions
source("/censocdev/R/load_dmf_deaths.R")
source("/censocdev/R/helpers.R")
source("/censocdev/R/calculate_age_at_death.R")
source("/censocdev/R/split-by-bpl.R")
source("/censocdev/R/create_weights_censoc_dmf.R")

# Load libraries
library(data.table)
library(dplyr)
library(readr).

# Paths
census_raw      <- "/ipums-repo2022/1940/TSV/P.tsv"
bunmd_raw       <- "/censoc/data/numident/4_berkeley_unified_mortality_database/bunmd.csv"
dmf_raw1        <- "/home/ipums/josh-ipums/progs/ssdm/ssdm1"
dmf_raw2        <- "/home/ipums/josh-ipums/progs/ssdm/ssdm2"
dmf_raw3        <- "/home/ipums/josh-ipums/progs/ssdm/ssdm3"
out_path_census <- "/abe/data/1940-census-by-bpl"
out_path_bunmd  <- "/abe/data/bunmd-by-bpl"
out_path_dmf    <- "/abe/data/dmf"
log_path        <- "/abe/log"

path_to_out_file <- paste0(log_path, "/1-run-split-by-bpl.log")
log <- file(path_to_out_file, open="wt")
sink(log, type="message")

# Split 1940 census
census <- fread(census_raw)
split_var_census <- "BPL"
cols_to_keep_census <- c("NAMELAST", "NAMEFRST", "BIRTHYR", "BPL", "SEX", "SERIALP", "PERNUM", "HISTID", "MARST",
                         "AGE", "RACE")
split_by_bpl(out_path_census, census, split_var_census, cols_to_keep_census)
length(list.files()) # 393

# Split BUNMD
bunmd <- fread(bunmd_raw)
bunmd$census_age <- ifelse(bunmd$bmonth<4, 1940-bunmd$byear, 1940-bunmd$byear-1)
split_var_bunmd <- "bpl"
split_by_bpl(out_path_bunmd, bunmd, split_var_bunmd)
length(list.files()) # 312 (including bpl is NA, that is, bpl=99999)

# Save DMF as csv (DMF has no bpl information)
dmf_files = c(dmf_raw1, dmf_raw2, dmf_raw3)
dmf <- load_dmf_deaths(dmf_files)
dmf <- calculate_age_at_death(dmf)
dmf <- create_weights_censoc_dmf(dmf, cohorts = c(1895:1939), death_ages = c(65:100))
dmf$sex <- 1
dir.create(out_path_dmf)
setwd(out_path_dmf)
fwrite(dmf, paste0(out_path_dmf, "/dmf.csv"))

# Clean up after yourself
sink(type="message")
close(log)
rm(list = ls())

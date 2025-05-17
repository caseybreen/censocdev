#########################################
# created by UGUR YILDIRIM (2020-08-17) #
# revised by WON-TAK JOO (2022-11-10)   #
#########################################

# Source functions
library(here) #set project to censocdev directory
source(here("R/load_dmf_deaths.R"))
source(here("R/helpers.R"))
source(here("R/calculate_age_at_death.R"))
source(here("R/split-by-bpl.R"))
source(here("R/create_weights_censoc_dmf.R"))

# Load libraries
library(data.table)
library(dplyr)
library(readr)

# Paths
census_raw      <- "/global/scratch/p2p3/pl1_demography/ipums/ipums-repo2022/1940/TSV/P.tsv"
bunmd_raw       <- "/global/scratch/p2p3/pl1_demography/censoc_internal/data/numident/4_berkeley_unified_mortality_database/bunmd.csv"
dmf_raw1        <- "/global/scratch/p2p3/pl1_demography/censoc/input_data/dmf/ssdm1"
dmf_raw2        <- "/global/scratch/p2p3/pl1_demography/censoc/input_data/dmf/ssdm2"
dmf_raw3        <- "/global/scratch/p2p3/pl1_demography/censoc/input_data/dmf/ssdm3"
out_path_census <- "/global/scratch/p2p3/pl1_demography/censoc_internal/censoc-abe-implementation/data/1940-census-by-bpl"
out_path_bunmd  <- "/global/scratch/p2p3/pl1_demography/censoc_internal/censoc-abe-implementation/data/bunmd-by-bpl"
out_path_dmf    <- "/global/scratch/p2p3/pl1_demography/censoc_internal/censoc-abe-implementation/data/dmf"
log_path        <- "/global/scratch/p2p3/pl1_demography/censoc_internal/censoc-abe-implementation/data/log"

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

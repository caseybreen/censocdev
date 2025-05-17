####################################################
# Last revised by Maria Osborne (August 11, 2023)  #
####################################################
#' Read and Harmonize NCHS mortality microdata
#'
#'
#' This Script Parses and Harmonized NCHS
#' MCOD data from the year 2005 and later
#'
#' Can only be run on SRDC platform where restricted NCHS
#' data files are stored


# Packages
library(readr)
library(data.table)
library(dplyr)

# Input path
# Years of data: 2005 - 2020
# note: some 2021 data are provided but are incomplete and not processed here
nchs_data_root_path <- "/global/secure0/groups/dmog-aux01-access/nchs/20230731.Goldstein_extracted/"
data_file_paths <- c(paste0(nchs_data_root_path, "MortLG2005/MULT2005.USLmtGeo.txt"),
                     paste0(nchs_data_root_path, "MortLG2006/MULT2006.USLmtGeo.txt"),
                     paste0(nchs_data_root_path, "MortLG2007/MULT2007.USLmtGeo.txt"),
                     paste0(nchs_data_root_path, "MortLG2008/MULT2008.USLmtGeo.txt"),
                     paste0(nchs_data_root_path, "MortLG2009/MULT2009.USLmtGeo.txt"),
                     paste0(nchs_data_root_path, "MortLG2010/MULT2010.USLmtGeo.txt"),
                     paste0(nchs_data_root_path, "MortLG2011/MULT2011.USLmtGeo.txt"),
                     paste0(nchs_data_root_path, "MortLG2012/MULT2012.USLmtGeo.txt"),
                     paste0(nchs_data_root_path, "MortLG2013/MULT2013.USLmtGeo.txt"),
                     paste0(nchs_data_root_path, "MortLG2014/MULT2014.USLmtGeo.txt"),
                     paste0(nchs_data_root_path, "MortLG2015/MULT2015.USLmtGeo.txt"),
                     paste0(nchs_data_root_path, "MortLG2016/MULT2016.USLmtGeo.txt"),
                     paste0(nchs_data_root_path, "MortLG2017/MULT2017US.LimGeo.txt"),
                     paste0(nchs_data_root_path, "MortLG2018/MULT2018US.LimGeo.txt"),
                     paste0(nchs_data_root_path, "MortLG2019/MULT2019.LimGeo.txt"),
                     paste0(nchs_data_root_path, "MortLG2020/Mort2020US.LimGeo.txt"))

fwf_metadata_path <- "/global/home/users/mariaosborne/CenSoc Weights/data/nchs_fw_layout.csv"

# Output paths
path_out <- "/global/home/users/mariaosborne/CenSoc Weights/data/nchs_restricted/"


# Read NCHS fixed width file
fwf_metadata <- fread(fwf_metadata_path)
colnames <- fwf_metadata$name
pos_starts <- fwf_metadata$location_start
pos_ends <- fwf_metadata$location_end

vars_to_select <- c("data_year", "sex", "state_occurrence_FIPS", "birthplace_recode",
                    "detail_age", "race_recode_3")

mcod_dat <- lapply(data_file_paths,
                  function(i) {read_fwf(file = i,
                  col_positions = fwf_positions(start = pos_starts, end = pos_ends, col_names = colnames),
                  col_select = all_of(vars_to_select))})

mcod_dat <- data.table::rbindlist(mcod_dat)

# Filter out any deaths occurring outside the US and DC
# (should not drop anything but just in case)
nrow(mcod_dat)
mcod_dat <- mcod_dat[state_occurrence_FIPS %in% c(state.abb, "DC")]

# Process age at death (4 digit)
mcod_dat[detail_age %/% 1000 == 1, death_age_years := detail_age %% 1000]
# if death_age_years is NA after this, individual died before age 1 (or death age is unknown)
table(mcod_dat$death_age_years, useNA = "always")
# remove 999 death age
mcod_dat[death_age_years == 999, death_age_years := NA]
summary(mcod_dat$death_age_years)


# Process/harmonize sex
mcod_dat[sex == "M", sex_string := "male"]
mcod_dat[sex == "F", sex_string := "female"]
table(mcod_dat$sex_string, useNA = "always")


# process race
table(mcod_dat$race_recode_3)
mcod_dat[race_recode_3 == 1, race_string := "white"]
mcod_dat[race_recode_3 == 2, race_string := "other"]
mcod_dat[race_recode_3 == 3, race_string := "black"]



# Harmonize birthplace
mcod_dat[, birthplace_recode := toupper(birthplace_recode)] # fix a couple of lowercase strings
states_convert <- data.frame("state_name" = state.name, "state_abb" = state.abb)
mcod_dat <- merge(x= mcod_dat, y=states_convert, by.x="birthplace_recode", by.y="state_abb", all.x=T)
mcod_dat[, bpl := state_name]
mcod_dat$state_name <- NULL
mcod_dat[birthplace_recode == "DC", bpl := "District of Columbia"]
#territories
# PR		... Puerto Rico
# VI		... Virgin Islands
# GU		... Guam
# AS		... American Samoa
# MP		... Northern Marianas
mcod_dat[birthplace_recode == "AS", bpl := "American Samoa"]
mcod_dat[birthplace_recode == "VI", bpl := "Virgin Islands"]
mcod_dat[birthplace_recode == "GU", bpl := "Guam"]
mcod_dat[birthplace_recode == "PR", bpl := "Puerto Rico"]
mcod_dat[birthplace_recode == "MP", bpl := "Northern Marianas"]
# other nations
# CC		           ... Canada
# MX		           ... Mexico
# CU		           ... Cuba
# YY		           ... Remainder of the world
# ZZ		           ... Unknown place of birth
mcod_dat[birthplace_recode == "CC", bpl := "Canada"]
mcod_dat[birthplace_recode == "CU", bpl := "Cuba"]
mcod_dat[birthplace_recode == "MX", bpl := "Mexico"]
mcod_dat[birthplace_recode == "YY", bpl := "Other Foreign Born"]
# ZZs will be NA


table(mcod_dat$bpl, useNA = "always") # make sure nothing here looks weird


# Code birthplace keys how we want
other_territories <- c("Guam", "Virgin Islands", "American Samoa", "Northern Marianas")
mcod_dat[bpl %in% other_territories, bpl_key := "Other US Territory"]
mcod_dat[!(bpl %in% other_territories), bpl_key := bpl]

# standardize bpl_key string
mcod_dat[, bpl_key := tolower(gsub(" ", "", bpl_key))] # remove spaces and capitals
table(mcod_dat$bpl_key)

# create flags for types of birthplaces
contig_us <- setdiff(state.name, c("Alaska", "Hawaii"))
mcod_dat[, bpl_flag_usa := as.integer(bpl %in% c(contig_us, "District of Columbia"))] # US exludes AK, HI
mcod_dat[, bpl_flag_territory := as.integer(bpl %in% c("Puerto Rico", other_territories))]
mcod_dat[, bpl_flag_AKHI := as.integer(bpl %in% c("Alaska", "Hawaii"))]
mcod_dat[, bpl_flag_foreign := as.integer(bpl %in% c("Canada", "Cuba", "Mexico", "Other Foreign Born"))]
mcod_dat[, bpl_flag_missing := as.integer(is.na(bpl))]

# make sure these add up to 1 (all records got a flag)
mean(mcod_dat$bpl_flag_AKHI==1) + mean(mcod_dat$bpl_flag_usa==1) +
  mean(mcod_dat$bpl_flag_territory==1) + mean(mcod_dat$bpl_flag_foreign==1) +
  mean(mcod_dat$bpl_flag_missing==1)


# Rename year var
mcod_dat[, year := data_year]


# Create keys for weighting
mcod_dat[, key := paste(paste0("a", death_age_years),
                         paste0("y", year),
                         paste0("s", sex_string),
                         paste0("r", race_string),
                         paste0("b", bpl_key),
                         sep = "_")]


# Save resulting data set
write_csv(mcod_dat, paste0(path_out, "/NCHS_data_merged_2005_to_2020.csv"))



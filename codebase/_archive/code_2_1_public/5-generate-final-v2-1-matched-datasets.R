#########################################
# created by UGUR YILDIRIM (2020-10-02) #
# revised by WON-TAK JOO (2022-11-10)   #
#########################################

# Source functions
source("/censocdev/R/create_weights_censoc_numident.R")
source("/censocdev/R/create_weights_censoc_dmf.R")

# Load libraries
library(data.table)
library(dplyr)
library(readr)

# Paths
matched_bunmd <- "/abe/data/matched-census-bunmd/matched_census_bunmd.csv"
matched_dmf   <- "/abe/data/matched-census-dmf/matched_census_dmf.csv"
out_path      <- "/abe/data/final-v2-1-matched-datasets"
dir.create(out_path)

# Read in Census - BUNMD matches
matched_census_bunmd <- fread(matched_bunmd)

# Restrict deaths to 1988-2005, births to <= 1940
matched_census_bunmd <- subset(matched_census_bunmd, dyear_B %in% 1988:2005)
matched_census_bunmd <- subset(matched_census_bunmd, byear_B <= 1940)

# Inspect matches

# We would lose 55200 (94159 without restrictions above) cases if we subset to matching bpl's only
#matched_census_bunmd <- subset(matched_census_bunmd, flag_bpl_NA_in_B == 0 & flag_bpl_only_in_A == 0 & flag_bpl_only_in_B == 0)

# Standard                                                                           Take 1      Take 2      Take 3
nrow(matched_census_bunmd)                                                         # 9785286 --> 9876987 --> 7890991

# Conservative +- 1 year
nrow(subset(matched_census_bunmd, uniquestub_match1 == 1))                         # 8444172 --> 8512378 --> 6801647
nrow(subset(matched_census_bunmd,                          uniquestub_file1 == 1)) # 8013159 --> 8071996 --> 6459796
nrow(subset(matched_census_bunmd, uniquestub_match1 == 1 & uniquestub_file1 == 1)) # 8013159 --> 8071996 --> 6459796

# Conservative +- 2 years
nrow(subset(matched_census_bunmd, uniquestub_match2 == 1))                         # 7474546 --> 7526783 --> 6018395
nrow(subset(matched_census_bunmd,                          uniquestub_file2 == 1)) # 7405515 --> 7453558 --> 5970178
nrow(subset(matched_census_bunmd, uniquestub_match2 == 1 & uniquestub_file2 == 1)) # 7192705 --> 7237908 --> 5797286

# Generate link_abe_exact_standard and link_abe_exact_conservative columns
matched_census_bunmd$link_abe_exact_standard <- 1
matched_census_bunmd$link_abe_exact_conservative <- ifelse(matched_census_bunmd$uniquestub_match2 == 1 & 
                                                             matched_census_bunmd$uniquestub_file2  == 1, 1, 0)

# Keep necessary variables only, rename
censoc_numident_v2_1 <- matched_census_bunmd[,c("id_A",
                                               "byear_B", "bmonth_B",
                                               "dyear_B", "dmonth_B", "death_age_B", "census_age_B",
                                               "sex_B",
                                               "race_first_B", "race_first_cyear_B", "race_first_cmonth_B",
                                               "race_last_B",  "race_last_cyear_B",  "race_last_cmonth_B",
                                               "bpl_B", "zip_residence_B", "socstate_B", "age_first_application_B",
                                               "link_abe_exact_standard", "link_abe_exact_conservative",
                                               "timediff_B",
                                               "flag_bpl_only_in_A", "flag_bpl_only_in_B", "flag_bpl_NA_in_B")]

colnames(censoc_numident_v2_1) <- c("HISTID",
                                    "byear", "bmonth",
                                    "dyear", "dmonth", "death_age", "census_age",
                                    "sex",
                                    "race_first", "race_first_cyear", "race_first_cmonth",
                                    "race_last",  "race_last_cyear",  "race_last_cmonth",
                                    "bpl", "zip_residence", "socstate", "age_first_application",
                                    "link_abe_exact_standard", "link_abe_exact_conservative",
                                    "byeardiff_census_minus_numident",
                                    "flag_bpl_only_in_census", "flag_bpl_only_in_numident", "flag_bpl_NA_in_numident")

# Generate weight variables
censoc_numident_v2_1 <- create_weights_censoc_numident(censoc_numident_v2_1, cohorts = c(1895:1939), death_ages = c(65:100))

temp <- subset(censoc_numident_v2_1, link_abe_exact_conservative == 1)
temp$weight <- NULL
temp <- create_weights_censoc_numident(temp, cohorts = c(1895:1939), death_ages = c(65:100))
temp <- temp[,c("HISTID", "weight")]
colnames(temp) <- c("HISTID", "weight_conservative")

censoc_numident_v2_1 <- censoc_numident_v2_1 %>% left_join(temp, by = "HISTID")

# Save final version of matches
censoc_numident_v2_1 <- censoc_numident_v2_1[,c("HISTID",
                                               "byear", "bmonth",
                                               "dyear", "dmonth", "death_age", "census_age",
                                               "sex",
                                               "race_first", "race_first_cyear", "race_first_cmonth",
                                               "race_last",  "race_last_cyear",  "race_last_cmonth",
                                               "bpl", "zip_residence", "socstate", "age_first_application",
                                               "link_abe_exact_standard", "link_abe_exact_conservative", 
                                               "byeardiff_census_minus_numident", 
                                               "flag_bpl_only_in_census", "flag_bpl_only_in_numident", "flag_bpl_NA_in_numident",
                                               "weight", "weight_conservative")]

fwrite(censoc_numident_v2_1, paste0(out_path, "/censoc_numident_v2_1.csv"))
rm(matched_census_bunmd, censoc_numident_v2_1, temp)



# Read in Census - DMF matches
matched_census_dmf <- fread(matched_dmf)

# Restrict deaths to 1975-2005, births to <= 1940 (no effect in this case)
matched_census_dmf <- subset(matched_census_dmf, dyear_B %in% 1975:2005)
matched_census_dmf <- subset(matched_census_dmf, byear_B <= 1940)

# Inspect matches

# Standard
nrow(matched_census_dmf)                                                         # 7419425

# Conservative +- 1 year
nrow(subset(matched_census_dmf, uniquestub_match1 == 1))                         # 5721679
nrow(subset(matched_census_dmf,                          uniquestub_file1 == 1)) # 5311192
nrow(subset(matched_census_dmf, uniquestub_match1 == 1 & uniquestub_file1 == 1)) # 5311192

# Conservative +- 2 years
nrow(subset(matched_census_dmf, uniquestub_match2 == 1))                         # 4740058
nrow(subset(matched_census_dmf,                          uniquestub_file2 == 1)) # 4714708
nrow(subset(matched_census_dmf, uniquestub_match2 == 1 & uniquestub_file2 == 1)) # 4510148

# Generate link_abe_exact_standard and link_abe_exact_conservative columns
matched_census_dmf$link_abe_exact_standard <- 1
matched_census_dmf$link_abe_exact_conservative <- ifelse(matched_census_dmf$uniquestub_match2 == 1 & 
                                                           matched_census_dmf$uniquestub_file2  == 1, 1, 0)

# Keep necessary variables only, rename
censoc_dmf_v2_1 <- matched_census_dmf[,c("id_A", "byear_B", "bmonth_B", "dyear_B", "dmonth_B", "death_age_B", "census_age_B",
                                        "link_abe_exact_standard", "link_abe_exact_conservative",
                                        "timediff_B")]

colnames(censoc_dmf_v2_1) <- c("HISTID", "byear", "bmonth", "dyear", "dmonth", "death_age", "census_age",
                               "link_abe_exact_standard", "link_abe_exact_conservative",
                               "byeardiff_census_minus_dmf")

# Generate weight variables
censoc_dmf_v2_1 <- create_weights_censoc_dmf(censoc_dmf_v2_1, cohorts = c(1895:1939), death_ages = c(65:100))

temp <- subset(censoc_dmf_v2_1, link_abe_exact_conservative == 1)
temp$weight <- NULL
temp <- create_weights_censoc_dmf(temp, cohorts = c(1895:1939), death_ages = c(65:100))
temp <- temp[,c("HISTID", "weight")]
colnames(temp) <- c("HISTID", "weight_conservative")

censoc_dmf_v2_1 <- censoc_dmf_v2_1 %>% left_join(temp, by = "HISTID")

# Save final version of matches
censoc_dmf_v2_1 <- censoc_dmf_v2_1[,c("HISTID", "byear", "bmonth", "dyear", "dmonth", "death_age", "census_age",
                                     "link_abe_exact_standard", "link_abe_exact_conservative",
                                     "byeardiff_census_minus_dmf",
                                     "weight", "weight_conservative")]

fwrite(censoc_dmf_v2_1, paste0(out_path, "/censoc_dmf_v2_1.csv"))
rm(matched_census_dmf, censoc_dmf_v2_1, temp)

rm(out_path, create_weights_censoc_numident, create_weights_censoc_dmf)

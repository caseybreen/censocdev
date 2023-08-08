# Generate BUNMD/enlistment final matched data set
# Author: Maria Osborne
# Updated: July 11, 2023

library(data.table)
library(dplyr)
library(readr)
library(stringi)
library(stringr)

# Cleaned enlistment data
path_full_enlistment <- "/data/censoc/workspace/enlistment_records/finalenlistment_for_linking.csv"

# Enlistment-Numident matches
path_enlistment_numident_matched <- "~mariaosborne-ipums/enlistment_records_linking/matched-enlistment-numident-males/matched_numident_enlistment_male.csv"

# Enlistment-census matches
path_census_enlistment_matched <- "~mariaosborne-ipums/enlistment_records_linking/matched_enlistment_datasets/internal_census_enlistment_links.csv"

# Final matched datasets out path
path_final <- "~mariaosborne-ipums/enlistment_records_linking/matched_enlistment_datasets/"


# Read in files
enlistment <- fread(path_full_enlistment)
numident_matches <- fread(path_enlistment_numident_matched)
census_to_enlistment_matches <- fread(path_census_enlistment_matched,
                                      select = c("id_A", "id_B", "fname_clean_std", "lname.y")) #all conservative matches
setnames(census_to_enlistment_matches, c("HISTID", "id_B", "fname_census_clean", "lname_census_clean"))

# Add unique row number identifier to enlistment records
enlistment <- enlistment %>% mutate(unique_ID = row_number())

# create conservative match flag for enlistment-numident matches
numident_matches$link_abe_exact_conservative_enlist_numident <- ifelse(
  numident_matches$uniquestub_match2 == 1 & numident_matches$uniquestub_file2 == 1, 1, 0)

# pare down to conservative matches
numident_matches <- numident_matches %>%
  filter(link_abe_exact_conservative_enlist_numident == 1) # 2.74 million --> 2.1 million

# id_A = SSN (From Numident)
# if_B = enlistment unique_ID

# Join enlistment variables back to linked enlistment/numident file
numident_matches_with_enlist_vars <- left_join(numident_matches, enlistment,
                                          by = c("id_B" = "unique_ID"), suffix = c("_NUMIDENT", "_ENLISTMENT"))

# Join census links
numident_matches_with_enlist_vars_and_histid <-
  left_join(numident_matches_with_enlist_vars, census_to_enlistment_matches, by = "id_B")

# how many census matches did we get?
sum(!is.na(numident_matches_with_enlist_vars_and_histid$HISTID)) # 943,723, about 45% of records

# save this for internal use
write_csv(numident_matches_with_enlist_vars_and_histid,
          paste0(path_final, "internal_numident_enlistment_links_male.csv"))


## make a publishable version with numident & enlistment vars, HISTID, no other identifiers ##

# select variables
# (note: birth year is present in both datasets)
numident_with_links_public <- numident_matches_with_enlist_vars_and_histid %>%
  select(bpl, byear_A, sex_A, bmonth_A, dyear_A, dmonth_A, death_age_A,
         race_first_A, race_first_cyear_A, race_first_cmonth_A, race_last_A,
         race_last_cyear_A, race_last_cmonth_A, zip_residence_A, socstate_A, age_first_application_A,
         byear_B, date_of_enlistment, residence_county, residence_state,
         place_of_enlistment, education, grade_code, branch_code, term_of_enlistment,
         race, citizenship, civilian_occupation, marital_status, height, weight_before_march_1943,
         weight_or_AGCT, component, source, HISTID)


# Limit to death years 1988-2005
numident_with_links_public <- numident_with_links_public %>%
  filter(dyear_A %in% 1988:2005) #2.1 million --> 1.69 million

# make a unique identifier for each row
set.seed(4886)
linked_numident_id <- stri_paste(
  stri_rand_strings(n=nrow(numident_with_links_public), 3, '[A-Za-z0-9]'),
  "-",
  stri_rand_strings(n=nrow(numident_with_links_public), 3, '[A-Za-z0-9]'),
  "-",
  stri_rand_strings(n=nrow(numident_with_links_public), 3, '[A-Za-z0-9]'))
length(unique(linked_numident_id)) == nrow(numident_with_links_public) # make sure all unique

numident_with_links_public$id <- linked_numident_id

# rename variables
# 1) replace _A with _numident
colnames(numident_with_links_public) = str_replace(colnames(numident_with_links_public), "_A$", "_numident")
# 2) add_enlistment_suffix to enlistment vars that don't have that already
colnames(numident_with_links_public)[17:34] <- paste0(colnames(numident_with_links_public)[17:34], "_enlistment")
# 3) special cases
numident_with_links_public <- numident_with_links_public %>% rename(sex = "sex_numident")
#numident_with_links_public <- numident_with_links_public %>% rename(bpl = "bpl_B")
numident_with_links_public <- numident_with_links_public %>% rename(byear_enlistment = byear_B_enlistment)

# rearrange columns
numident_with_links_public <- numident_with_links_public %>% relocate(sex)
numident_with_links_public <- numident_with_links_public %>% relocate(id)

# check
head(numident_with_links_public)
names(numident_with_links_public)

# write
write_csv(numident_with_links_public,
          paste0(path_final, "public_numident_enlistment_links_males.csv"))

# clean up
rm(list = ls())



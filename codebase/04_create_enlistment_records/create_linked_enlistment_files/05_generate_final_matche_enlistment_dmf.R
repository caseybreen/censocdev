# generate DMF/enlistment final matches
# Author: Maria Osborne
# Updated: July 2, 2023

library(data.table)
library(dplyr)
library(readr)
library(stringi)

# Cleaned enlistment data
path_full_enlistment <- "/data/censoc/workspace/enlistment_records/finalenlistment.csv"

# Enlistment-DMF matches
path_enlistment_dmf_matched <- "~mariaosborne-ipums/enlistment_records_linking/matched_enlistment_dmf/matched_enlistment_dmf.csv"

# Enlistment-census matches
path_census_enlistment_matched <- "~mariaosborne-ipums/enlistment_records_linking/matched_enlistment_datasets/internal_census_enlistment_links.csv"

# Final matched datasets out path
path_final <- "~mariaosborne-ipums/enlistment_records_linking/matched_enlistment_datasets/"


# Read in files
enlistment <- fread(path_full_enlistment)
dmf_matches <- fread(path_enlistment_dmf_matched)
census_to_enlistment_matches <- fread(path_census_enlistment_matched,
                                      select = c("id_A", "id_B", "fname.y", "lname.y")) #all conservative matches
setnames(census_to_enlistment_matches, c("HISTID", "id_B", "fname_census_clean", "lname_census_clean"))

# Add unique row number identifier to enlistment records
enlistment <- enlistment %>% mutate(unique_ID = row_number())

# create conservative match flag for enlistment-numident matches
dmf_matches$link_abe_exact_conservative_enlist_dmf <- ifelse(
  dmf_matches$uniquestub_match2 == 1 & dmf_matches$uniquestub_file2 == 1, 1, 0)

# pare down to consdrevative matches
dmf_matches <- dmf_matches %>% filter(link_abe_exact_conservative_enlist_dmf == 1) # 2.9 million -> 1.85 million records


# id_A = SSN (From DMF)
# if_B = enlistment unique_ID

# Join enlistment variables back to linked enlistment/dmf file
dmf_matches_with_enlist_vars <- left_join(dmf_matches, enlistment,
                                        by = c("id_B" = "unique_ID"), suffix = c("_DMF", "_ENLISTMENT"))

# Join census links
dmf_matches_with_enlist_vars_and_histid <-
  left_join(dmf_matches_with_enlist_vars, census_to_enlistment_matches, by = "id_B")

# how many matches did we get?
sum(!is.na(dmf_matches_with_enlist_vars_and_histid$HISTID)) # 779,389, about 42%

# save this for internal use
write_csv(dmf_matches_with_enlist_vars_and_histid,
          paste0(path_final, "internal_dmf_enlistment_links.csv"))


## make a publishable version with DMF & enlistment vars, HISTID, no other identifiers ##

# select variables
# (note: birth year is present in both datasets)
dmf_with_links_public <- dmf_matches_with_enlist_vars_and_histid %>%
  select(byear_A, sex_A, bmonth_A, dyear_A, dmonth_A, death_age_A,
         byear, date_of_enlistment, bpl, residence_county, residence_state,
         place_of_enlistment, education, grade_code, branch_code, term_of_enlistment,
         race, citizenship, civilian_occupation, marital_status, height, weight_before_march_1943,
         weight_or_AGCT, component, source, HISTID)

# make a unique identifier for each row 
set.seed(4486)
linked_dmfs_id <- stri_paste(
  stri_rand_strings(n=nrow(dmf_with_links_public), 4, '[A-Za-z0-9]'),
  "-",
  stri_rand_strings(n=nrow(dmf_with_links_public), 4, '[A-Za-z0-9]'))
length(unique(linked_dmfs_id)) == nrow(dmf_with_links_public) # make sure all unique

dmf_with_links_public$id <- linked_dmfs_id

# rename variables
# 1) replace _A with _DMF
colnames(dmf_with_links_public) = str_replace(colnames(dmf_with_links_public), "_A$", "_DMF")
# 2) add_enlistment_suffix to enlistment vars that don't have that already
colnames(dmf_with_links_public)[7:25] <- paste0(colnames(dmf_with_links_public)[7:25], "_enlistment")
# 3) special cases
dmf_with_links_public <- dmf_with_links_public %>% rename(sex = "sex_DMF")
dmf_with_links_public <- dmf_with_links_public %>% rename(HISTID = "HISTID_census_1940")

# rearrange columns
dmf_with_links_public <- dmf_with_links_public %>% relocate(sex)
dmf_with_links_public <- dmf_with_links_public %>% relocate(id)

# check
head(dmf_with_links_public)

# write
write_csv(dmf_with_links_public,
          paste0(path_final, "public_dmf_enlistment_links.csv"))

# clean up
rm(list = ls())



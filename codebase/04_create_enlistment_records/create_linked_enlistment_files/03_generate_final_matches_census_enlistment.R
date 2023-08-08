# Generate final matches to census
# Author: Maria Osborne
# Updated: June 24, 2023

library(dplyr)
library(readr)
library(data.table)

# Cleaned enlistment data
path_full_enlistment <- "/data/censoc/workspace/enlistment_records/finalenlistment_for_linking.csv"

# Enlistment-Census matches
path_enlistment_census_matched <- "~mariaosborne-ipums/enlistment_records_linking/matched-enlistment-census/matched_census_enlistment_male.csv"

# Out path Final matched datasets
path_final <- "~mariaosborne-ipums/enlistment_records_linking/matched_enlistment_datasets/"


# Read in files
enlistment <- fread(path_full_enlistment)
census_matches <- fread(path_enlistment_census_matched)

# Add unique row number identifier to enlistment records
enlistment <- enlistment %>% mutate(unique_ID = row_number())

# create conservative match flag for enlistment-numident matches
census_matches$link_abe_exact_conservative_enlist_census <- ifelse(
  census_matches$uniquestub_match2 == 1 & census_matches$uniquestub_file2 == 1, 1, 0)

# pare down to conservative matches and key linking variables
census_matches<- census_matches %>% filter(link_abe_exact_conservative_enlist_census == 1)

# Add enlistment identifiers to census where there are matches
census_with_links <- left_join(census_matches, enlistment, by = c("id_B" = "unique_ID"))

# save this as an internal file (with identifying info) for quality checks
write_csv(census_with_links,
          paste0(path_final, "internal_census_enlistment_links.csv"))



# make files with just HISTID and enlistment vars that can be linked onto published IPUMS census data
census_with_links_public <- left_join(census_matches %>% select(id_A, id_B),
                                          enlistment,
                                          by = c("id_B" = "unique_ID"))
# rename id_A to HISTID
census_with_links_public <- census_with_links_public %>% rename("HISTID" = id_A)

# remove identifying information from this version
census_with_links_public$id_B <- NULL
census_with_links_public$fname <- NULL
census_with_links_public$fname_clean_std <- NULL
census_with_links_public$mname <- NULL
census_with_links_public$lname <- NULL
census_with_links_public$serial_number <- NULL

# check
names(census_with_links_public)

# write de-identified version of file
write_csv(census_with_links_public,
          paste0(path_final, "public_census_enlistment_links.csv"))

# clean up
rm(list = ls())



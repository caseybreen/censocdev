#' This script tracks downloads of CenSoc Datasets from Harvard Dataverse.
#'
#' All downloaders of CenSoc data from Harvard Dataverse must sign a "guestbook"
#' where name, email, institution, and position are collected.
#' The guestbook can only be downloaded by admins of the dataverse.
#'
#' NOTE: for about 7/2023 through 9/2023, the guestbook was disabled for the CenSoc-DMF
#' and CenSoc-Numident. This was while our article in Scientific Data
#' (https://www.nature.com/articles/s41597-023-02713-y) was under review so that
#' reviewers who downloaded the data mentioned in that article remained anonymous.
#' This affects downloads of the CenSoc-Numident/CenSoc-DMF version 2.1 and codebooks.

library(tidyverse)
library(data.table)

# Read guestbook (note: read permissions limited)
gb <- fread("~/censoc/UC_Berkeley_CenSoc__GuestbookReponses.csv")

# Format date
gb[, c("mo", "day", "yr") := tstrsplit(Date, "/", fixed = TRUE)]
gb <- gb %>% mutate_at(c("mo", "day", "yr"), as.integer)


# Categorize downloads: first into groups....
table(gb$Dataset)
gb <- gb %>%
  mutate(dataset_group = case_when(
    str_detect(Dataset, pattern = "BUNMD") ~ "BUNMD",
    str_detect(Dataset, pattern = "Numident") ~ "CenSoc-Numident",
    str_detect(Dataset, pattern = "Demo") ~ "Demo File",
    str_detect(Dataset, pattern = "DMF") ~ "CenSoc-DMF",
    str_detect(Dataset, pattern = "Enlistment") ~ "Enlistment",
    TRUE ~ NA_character_))

# ... Then scrape individual datasets for exact stats
table(gb$`File Name`)
gb <- gb %>%
  mutate(dataset_downloaded = case_when(
    str_detect(`File Name`, pattern = "bunmd") ~ "BUNMD",
    str_detect(`File Name`, pattern = "numident") & !(str_detect(`File Name`, pattern = "demo")) &
                                                        !(str_detect(`File Name`, pattern = "enlistment")) ~ "CenSoc-Numident",
    str_detect(`File Name`, pattern = "numident_demo") ~ "CenSoc-Numident Demo",
    str_detect(`File Name`, pattern = "dmf") & !(str_detect(`File Name`, pattern = "demo")) &
                                                      !(str_detect(`File Name`, pattern = "enlistment")) ~ "CenSoc-DMF",
    str_detect(`File Name`, pattern = "dmf_demo") ~ "CenSoc-DMF Demo",
    str_detect(`File Name`, pattern = "censoc_wwii_army_enlistment") ~ "CenSoc WWII Enlistment",
    str_detect(`File Name`, pattern = "censoc_enlistment_census_1940") ~ "CenSoc-Census WWII Enlistment",
    str_detect(`File Name`, pattern = "enlistment_numident") ~ "CenSoc-Numident WWII Enlistment",
    str_detect(`File Name`, pattern = "enlistment_dmf") ~ "CenSoc-DMF WWII Enlistment",
    str_detect(`File Name`, pattern = "numident_enlistment") ~ "CenSoc-Numident WWII Enlistment",
    str_detect(`File Name`, pattern = "dmf_enlistment") ~ "CenSoc-DMF WWII Enlistment",
    TRUE ~ NA_character_))
# note: might need to deal with the fact that I had one of the
# enlistment codebooks misnamed for like a month.

# make sure this looks okay
gb %>% group_by(Dataset, dataset_group, dataset_downloaded) %>% tally()

# Remove downloads prior to 2023 when data became available
gb <- gb %>% filter(yr >= 2023)

# Flag codebook vs data downloads in case we care about that
gb <- gb %>% mutate(data_or_codebook =
               case_when(str_detect(`File Name`, pattern = "codebook") ~ "codebook",
                         TRUE ~ "dataset"))


# what are the most downloaded datasets?
gb %>% group_by(dataset_downloaded) %>% tally() %>% arrange(desc(n))
# Demo files, BUNMD, Enlistment files

# Get email addresses
#hvd_users <- gb %>% filter(Email != "") %>%
#  group_by(`User Name`, Email) %>%
#  tally() %>%
#  select(-n)





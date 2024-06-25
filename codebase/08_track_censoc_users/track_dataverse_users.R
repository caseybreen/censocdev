#' This script tracks downloads of CenSoc Datasets from Harvard Dataverse.
#'
#' All downloaders of CenSoc data from Harvard Dataverse must sign a "guestbook"
#' where name, email, institution, and position are collected.
#' The guestbook can only be downloaded by admins of the dataverse.
#' To acccess: log in to HDV, go to Berkeley CenSoc page, and select
#' 'Dataverse Guestbooks' from dropdown edit menu in upper right
#'
#' NOTE: From mid July to Mid October 2023, users were not required to provide information
#' when downloading the  CenSoc-DMF and CenSoc-Numident. This was so that reviewers of our
#' article in Scientific Data  (https://www.nature.com/articles/s41597-023-02713-y) could
#' remain anonymous while the paper was under review.
#' Downloads of the CenSoc-Numident and CenSoc-DMF during this time do appear to be tracked
#' in the guestbook, but no identifying information of the downloaders was recorded.

library(tidyverse)
library(data.table)
library(ggsci)
library(ggthemes)

# Read guestbook (note: read permissions limited)
gb <- fread("~/censoc/hdv_guestbooks/UC_Berkeley_CenSoc__GuestbookReponses_apr_29_2024.csv")
head(gb)

# Format date
gb[, c("mo", "day", "yr") := tstrsplit(Date, "/", fixed = TRUE)]
gb <- gb %>% mutate_at(c("mo", "day", "yr"), as.integer)

# Remove downloads prior to 2023 (earlier were internal tests)
gb <- gb %>% filter(yr >= 2023)


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
    str_detect(`File Name`, pattern = "bunmd") & !(str_detect(`File Name`, pattern = "supplement")) &
      !(str_detect(`File Name`, pattern = "names")) ~ "BUNMD",
    str_detect(`File Name`, pattern = "numident") & !(str_detect(`File Name`, pattern = "demo")) &
                                                      !(str_detect(`File Name`, pattern = "enlistment")) &
                                                      !(str_detect(`File Name`, pattern = "supplement")) ~ "CenSoc-Numident",
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
    str_detect(`File Name`, pattern = "supplement") ~ "Supplementary Files",
    str_detect(`File Name`, pattern = "names") ~ "Supplementary Files",
    TRUE ~ NA_character_))
gb %>% group_by(dataset_downloaded) %>% tally() %>% arrange(desc(n))

# make sure this looks okay
gb %>% group_by(Dataset, dataset_group, dataset_downloaded) %>% tally()

# Flag codebook vs data downloads in case we care about that
gb <- gb %>% mutate(data_or_codebook =
               case_when(str_detect(`File Name`, pattern = "codebook") ~ "codebook",
                         TRUE ~ "dataset"))


# which datasets & codebooks are most downloaded?
gb %>% group_by(dataset_downloaded) %>% tally() %>% arrange(desc(n))
# Demo files, BUNMD, Enlistment files

# "Type" can be download, explore, read doc, or a few other options
table(gb$Type)

# For download metrics, we will only care about actual downloads of datasets themselves
gb_metrics <- gb %>% filter(Type == "Download" & data_or_codebook == "dataset")

# Let's look at monthly download numbers
# formatting these as Date objects will allow us sort them chronologically
gb_metrics <- gb_metrics %>% mutate(date_obj = paste(str_pad(day ,2, pad ="0", side ="left"),
                                                     str_pad(mo,2, pad ="0", side ="left"),
                                                     yr, sep = "-")) %>%
  mutate(date_obj = format(as.Date(date_obj, "%d-%m-%Y"))) %>%
  mutate(year_mo = format(as.Date(date_obj), "%Y-%m"))

# Bar plot of downloads by month since June 2023
gb_metrics %>%
  filter(year_mo >= "2023-06") %>%
  group_by(year_mo) %>%
  tally() %>%
  ggplot(aes(year_mo, n)) +
  geom_col() +
  theme_classic() +
  labs(x = "Month",
       y = "Downloads") +
  scale_y_continuous(labels = scales::comma)

# monthly downloads categorizes by dataset group
gb_metrics %>% filter(year_mo >= "2023-06") %>%
  ggplot(aes(fill=dataset_group, x=year_mo)) +
  geom_bar(position="stack", stat="count") +
  theme_classic() +
  ggsci::scale_fill_locuszoom() +
  ggtitle("Dataset Downloads June 1 2023 - April 29, 2024") # change end date if needed

# Note: spike in demo downloads in 10/2023 are likely from a teaching demonstration
# Note: No user info for Numident & DMF downloads from about July - October 2023

# Let's plot downloads by dataset
gb_metrics %>%
  group_by(dataset_downloaded) %>%
  tally() %>%
  arrange(desc(n)) %>%
  ungroup %>%
  mutate(dataset_downloaded= reorder(dataset_downloaded, n)) %>%
  ggplot(aes(dataset_downloaded, n)) +
  geom_col() +
  coord_flip() +
  theme_classic()









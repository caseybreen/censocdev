#' Reattach weights to individual CenSoc records
#'
#' Note: CenSoc 3.0 datasets use the same linkages as CenSoc 2.1
#' If linkages are updated for further data version releases, paths
#' should be changed accordingly.
#'

# libraries
library(data.table)
library(dplyr)

# In paths
path_numident_weights <- "~mariaosborne-ipums/censoc_weights/transfer/numident_weights_by_strata.csv"
path_numident_with_keys <- "~mariaosborne-ipums/censoc_weights/data/numident/numident_data_prepped.csv"
path_numident_full <- "/data/censoc/censoc_data_releases/censoc_numident/censoc_numident_v2.1/censoc_numident_v2.1.csv"

path_dmf_weights <- "~mariaosborne-ipums/censoc_weights/transfer/dmf_weights_by_strata.csv"
path_dmf_with_keys <- "~mariaosborne-ipums/censoc_weights/data/DMF/dmf_data_prepped.csv"
path_dmf_full <- "/data/censoc/censoc_data_releases/censoc_dmf/censoc_dmf_v2.1/censoc_dmf_v2.1.csv"


# Out paths
path_out <- "~mariaosborne-ipums/censoc_weights/data/censoc_v3/"

# Read weights and data
numident_weights <- fread(path_numident_weights)
numident_with_keys <- fread(path_numident_with_keys, select = c("HISTID", "key"))
dmf_weights <- fread(path_dmf_weights)
dmf_with_keys <- fread(path_dmf_with_keys, select = c("HISTID", "key"))


# Attach IDs to weights
numident_id_with_wts <- left_join(numident_with_keys, numident_weights, by = "key") %>% 
  dplyr::select(-c(key))
dmf_id_with_wts <- left_join(dmf_with_keys, dmf_weights, by = "key") %>% dplyr::select(-c(key))


# Join to full datasets
numident_full <- fread(path_numident_full)
numident_full <- numident_full %>% filter(link_abe_exact_conservative == 1) %>% # restrict to conservative links
                  dplyr::select(-c(weight, weight_conservative, link_abe_exact_conservative)) # remove old weights
numident_full <- left_join(numident_full, numident_id_with_wts, by = "HISTID")  
numident_full[, weight := round(weight_final, 5)]
numident_full[, weight_final := NULL]

dmf_full <- fread(path_dmf_full)
dmf_full <- dmf_full %>% filter(link_abe_exact_conservative == 1) %>% 
                         dplyr::select(-c(weight, weight_conservative, link_abe_exact_conservative))
dmf_full <- left_join(dmf_full, dmf_id_with_wts, by = "HISTID")
dmf_full[, weight := round(weight_final, 5)]
dmf_full[, weight_final := NULL]


# make sure these look okay
table(numident_full[is.na(weight)]$death_age) # people w/o weights are either < 65 or > 100 years old
table(dmf_full[is.na(weight)]$death_age)

# write files
write_csv(numident_full, paste0(path_out, "censoc_numident_v3.csv"))
write_csv(dmf_full, paste0(path_out, "censoc_dmf_v3.csv"))




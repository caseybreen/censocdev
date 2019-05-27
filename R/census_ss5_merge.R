#' Merge SS5 and Census
#'
#' @param ss5 full ss5 (merged application and death file)
#' @return NUMAPP data.frame with best first name column for each unique ssn (40870456) and cycle dates
#' @keywords internal
#' @import data.table
#' @export
#'
#'
census_ss5_merge <- function(ss5 = ss5, census = census){

  ## Define census and SS5
  census <- census
  ss5 <- ss5

  ## omit rows where either 'bpl' or 'census_age' have missing values
  ss5 <- na.omit(ss5, cols=c("bpl", "census_age"))

  ## Create SS5 dataset with unique married (last name) keys (remove rows w dups)
  ss5_married_unique_keys <- ss5[ss5[, .I[.N == 1L], by=linking_key_married]$V1]
  ss5_married_unique_keys_women <- ss5_married_unique_keys[sex == 2]
  ss5_married_unique_keys_men <- ss5_married_unique_keys[sex == 1]

  ## Create SS5 dataset with unique maiden (father's last name) keys (remove rows w dups)
  ss5_maiden_unique_keys <- ss5[ss5[, .I[.N == 1L], by=linking_key_maiden]$V1]
  ss5_maiden_unique_keys <- ss5_maiden_unique_keys[sex == 2]

  ## Create Census dataset with unique key (remove rows w dups)
  census_unique_keys <- census[census[, .I[.N == 1L], by=linking_key]$V1]

  ## Sort Census into men, women ever-married, and women never-married
  census_men <- census_unique_keys[SEX == 1]
  census_women_never_married <- census_unique_keys[MARST == 6 & SEX == 2]
  census_women_ever_married <- census_unique_keys[MARST != 6 & SEX == 2]

  ## Merge women ever-married at the time of the 1940 census with SS-5 on married keys
  ss5_married_unique_keys_women[, "linking_key" :=  linking_key_married]
  wcensoc_married <- merge(census_women_ever_married, ss5_married_unique_keys_women, by = "linking_key")

  ## flag for merged with maiden name
  wcensoc_married[, "maiden_name_flag" := 0]

  ## Merge women never-married at the time of the 1940 census with SS-5 maiden key
  ss5_maiden_unique_keys[, "linking_key" :=  linking_key_maiden]
  wcensoc_maiden <-  merge(census_women_never_married, ss5_maiden_unique_keys, by = "linking_key")

  ## flag for merged with maiden name
  wcensoc_maiden[, "maiden_name_flag" := 1]

  ## Merge men on maiden name key
  ss5_married_unique_keys_men[, "linking_key" := linking_key_married]
  wcensoc_men <-  merge(census_men, ss5_married_unique_keys_men, by = "linking_key")

  ## flag for merged with maiden name
  ## wcensoc_men[, "maiden_name_flag" := 1]

  ## Create wcensoc file by taking all records from the merge on the married key and appending any additional records
  ## from the merge on the maiden name key not already in the married merge.

  ## Additional matches found using maiden name
  additional_matches_found_maiden_name <- wcensoc_maiden[!wcensoc_maiden$ssn %in% wcensoc_married$ssn]

  ## Create Women's CenSoc File
  wcensoc_women <- rbind(wcensoc_married, additional_matches_found_maiden_name, fill = TRUE)

  ## Combine Men and Women's CenSoc File
  wcensoc <- rbind(wcensoc_women, wcensoc_men, fill = TRUE)

  return(wcensoc)
}


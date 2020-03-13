#' Merge bunmd and Census
#'
#' @param bunmd full bunmd (merged application and death file)
#' @return NUMAPP data.frame with best first name column for each unique ssn (40870456) and cycle dates
#' @keywords internal
#' @import data.table
#' @export
#'
census_bunmd_merge <- function(bunmd = bunmd, census = census, census_year = 1940){


  ## create census age variable
  bunmd[,"census_age" := ifelse(bmonth < 4,
                                census_year - byear,
                                census_year-1 - byear)]

  ## filter out people born after census year
  bunmd <- bunmd[census_age >= 0]

  ## filter out names that are only one character

  bunmd <- bunmd %>%
    filter(nchar(lname) > 1) %>%
    filter(nchar(fname) > 1)

  ## set bunmd as data.table
  setDT(bunmd)

  ## omit rows where either 'bpl' or 'census_age' have missing values
  bunmd <- na.omit(bunmd, cols=c("bpl", "census_age"))

  ## clean first name variables
  bunmd[,"fname" := enc2native(fname)]
  bunmd[,"fname" := toupper(fname)]
  bunmd[,"fname" := get_first_word(fname)]

  ## clean last name variables
  bunmd[,"lname" := enc2native(lname)]
  bunmd[,"lname" := toupper(lname)]
  bunmd[,"lname" := get_first_word(lname)]

  ## Create two sets of linking keys (married name = lname, maiden name = father_lname)
  bunmd[,"linking_key_married" := paste(lname, fname, census_age, bpl, sep = "_")]
  bunmd[,"linking_key_married" := clean_key(linking_key_married),]

  bunmd[,"linking_key_maiden" := paste(father_lname, fname, census_age, bpl, sep = "_")]
  bunmd[,"linking_key_maiden" := clean_key(linking_key_maiden),]

  ## Create bunmd dataset with unique married keys for women (remove rows w dupes)
  bunmd_married_unique_keys_women <- bunmd[sex == 2]
  bunmd_married_unique_keys_women <- bunmd_married_unique_keys_women[bunmd_married_unique_keys_women[, .I[.N == 1L], by=linking_key_married]$V1]

  ## Create bunmd dataset with unique married keys for men (remove rows w dupes)
  bunmd_married_unique_keys_men <- bunmd[sex == 1]
  bunmd_married_unique_keys_men <- bunmd_married_unique_keys_men[bunmd_married_unique_keys_men[, .I[.N == 1L], by=linking_key_married]$V1]


  ## Create bunmd dataset with unique maiden (father's last name) keys (remove rows w dups)
  bunmd_maiden_unique_keys <- bunmd[bunmd[, .I[.N == 1L], by=linking_key_maiden]$V1]
  bunmd_maiden_unique_keys <- bunmd_maiden_unique_keys[sex == 2]

  ## Sort Census into men, women ever-married, and women never-married
  census_men <- census[SEX == 1]
  census_men <- census_men[census_men[, .I[.N == 1L], by=linking_key]$V1]

  census_women_never_married <- census[MARST == 6 & SEX == 2]
  census_women_never_married <- census_women_never_married[census_women_never_married[, .I[.N == 1L], by=linking_key]$V1]

  census_women_ever_married <- census[MARST != 6 & SEX == 2]
  census_women_ever_married <- census_women_ever_married[census_women_ever_married[, .I[.N == 1L], by=linking_key]$V1]

  ## Merge women ever-married at the time of the 1940 census with SS-5 on married keys
  bunmd_married_unique_keys_women[, "linking_key" :=  linking_key_married]
  wcensoc_married <- merge(census_women_ever_married, bunmd_married_unique_keys_women, by = "linking_key")

  ## flag for merged with maiden name
  wcensoc_married[, "maiden_name_flag" := 0]

  ## Merge women never-married at the time of the 1940 census with SS-5 maiden key
  bunmd_maiden_unique_keys[, "linking_key" :=  linking_key_maiden]
  wcensoc_maiden <-  merge(census_women_never_married, bunmd_maiden_unique_keys, by = "linking_key")

  ## flag for merged with maiden name
  wcensoc_maiden[, "maiden_name_flag" := 1]

  ## Merge men on maiden name key
  bunmd_married_unique_keys_men[, "linking_key" := linking_key_married]
  wcensoc_men <-  merge(census_men, bunmd_married_unique_keys_men, by = "linking_key")

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


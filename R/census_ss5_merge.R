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

  ## Remove duplicates for both SS5 married key and maiden name key
  ss5_married_unique_keys <- ss5[ss5[, .I[.N > 1L], by=linking_key_married]$V1]

  ss5_maiden_unique_keys <- ss5[ss5[, .I[.N > 1L], by=linking_key_maiden]$V1]

  ## Select Linking keys for census
  census_unique_keys <- census[census[, .I[.N > 1L], by=linking_key]$V1]

  ## Read in linking keys
  wcensoc_married <- merge(census_unique_keys, ss5_married_unique_keys, by.x = "linking_key", by.y = "linking_key_married")

  ## Create flag
  wcensoc_married[, "maiden_name_flag" := 0]

  wcensoc_maiden <-  merge(census_unique_keys, ss5_maiden_unique_keys, by.x = "linking_key", by.y = "linking_key_maiden")

  wcensoc_maiden[, "maiden_name_flag" := 1]

  wcensoc <- rbind(wcensoc_married, wcensoc_married)

  return(wcensoc)
}


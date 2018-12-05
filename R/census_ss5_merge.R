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

  ## Remove duplicate linking keys
  ss5 <- ss5[ss5[, .I[.N > 1L], by=linking_key]$V1]
  census <- census[census[, .I[.N > 1L], by=linking_key]$V1]

  ## Read in linking keys
  wcensoc <- merge(census, ss5, by = "linking_key")


}


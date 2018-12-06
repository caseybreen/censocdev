#' Merge Application and Death Files
#'
#' @param numapp path to the NUMAPP files
#' @return NUMAPP data.frame with best first name column for each unique ssn (40870456) and cycle dates
#' @keywords internal
#' @import data.table
#' @export
#'
merge_numapp_numdeath <- function(numapp_condensed = numapp, numdeath = numdeath) {

  numapp <- numapp
  numdeath2 <- numdeath[, c("ssn", "zip_residence", "dyear", "dmonth", "byear_death_file", "bmonth_death_file", "dstate", "socstate"), with = FALSE]
  ss5 <- merge(numdeath2, numapp, by = "ssn")

  ss5[,"census_age" := ifelse(bmonth < 4,
                               1940 - byear,
                               1939 - byear)]

  ## Create two sets of linking keys (married name = lname, maiden name = father_lname)
  ss5[,"linking_key_married" := paste(lname, fname, census_age, ipums_code, sep = "_")]

  ss5[,"linking_key_maiden" := paste(father_lname, fname, census_age, ipums_code, sep = "_")]

  return(ss5)
}

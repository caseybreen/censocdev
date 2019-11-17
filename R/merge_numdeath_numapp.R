#' Merge Application and Death Files
#'
#' @param numapp path to the NUMAPP files
#' @return NUMAPP data.frame with best first name column for each unique ssn (40870456) and cycle dates
#' @keywords internal
#' @import data.table
#' @export
#'
merge_numapp_numdeath <- function(numapp = numapp, numdeath = numdeath, census_year = 1940) {

  numapp <- numapp
  numdeath <- numdeath[, c("ssn", "zip_residence", "dyear", "dmonth", "byear_death_file", "bmonth_death_file", "dstate", "socstate"), with = FALSE]
  ss5 <- merge(numdeath, numapp, by = "ssn")

  ss5[,"census_age" := ifelse(bmonth < 4,
                              census_year - byear,
                              census_year-1 - byear)]

  ## filter out people born after census year
  ss5 <- ss5[census_age >= 0]

  ## Create two sets of linking keys (married name = lname, maiden name = father_lname)

  ss5[,"linking_key_married" := paste(lname, fname, census_age, bpl, sep = "_")]
  ss5[,"linking_key_married" := clean_key(linking_key_married),]

  ss5[,"linking_key_maiden" := paste(father_lname, fname, census_age, bpl, sep = "_")]
  ss5[,"linking_key_maiden" := clean_key(linking_key_maiden),]

  return(ss5)
}


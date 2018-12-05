#' Merge Application and Death Files
#'
#' @param numapp path to the NUMAPP files
#' @return NUMAPP data.frame with best first name column for each unique ssn (40870456) and cycle dates
#' @keywords internal
#' @import data.table
#' @export
#'
merge_numapp_numdeath <- function(numapp = numapp, numdeath = numdeath) {

  numapp <- numapp
  numdeath <- numdeath

  ss5 <- merge(numdeath, numapp, by = "ssn")

  ss5[,"census_age" := ifelse(bmonth < 4,
                               1940 - byear,
                               1939 - byear)]

  ss5[,"linking_key" := paste(lname, fname, census_age, BPL, sep = "_")]



  return(ss5)
}

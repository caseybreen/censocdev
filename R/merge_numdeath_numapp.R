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
  numdeath <- numdeath[, c("ssn", "zip_residence", "dyear", "dmonth", "byear_death_file", "bmonth_death_file", "dstate", "socstate"), with = FALSE]
  numapp <- numapp[, ssn:=as.integer(ssn)]
  
  ss5 <- merge(numdeath, numapp, by = "ssn")

 

  return(ss5)
}

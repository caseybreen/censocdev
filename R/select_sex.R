#' Select sex
#'
#' @param numapp path to the NUMAPP files
#' @return NUMAPP data.frame with best first name column and cycle dates
#' @keywords internal
#' @import data.table
#' @export

select_sex <- function(numapp = numapp) {


  numapp <- numapp[, c("ssn", "sex", "cycle_date", "year_cycle", "month_cycle"), with=FALSE]

  ## Clean & Create father_lname variable
  numapp[numapp$sex == 0]<-NA
  numapp <- na.omit(numapp, cols="sex")

  ## Remove applications with NA value for fname
  applications <- nrow(numapp)
  numapp <- na.omit(numapp, cols="sex")
  removed_na <- applications - nrow(numapp)
  cat(removed_na, "removed with 0 value (no information) for sex", "\n")

  ## Set missing values equal to 0. These will be selected last according to our selection process.
  for (col in c("year_cycle", "month_cycle")) numapp[is.na(get(col)), (col) := 0]

  ## Maybe should convert this to century months in the future?
  numapp[,"cycle_year_month" := year_cycle + (month_cycle/12)]

  ## Number of different sexes per SSN
  numapp[, number_of_distinct_sexes:=uniqueN(sex), by = ssn]

  ## Create flag (0 or 1 dichotomous var) for more than one first name.
  numapp[, fname_multiple_flag:=(ifelse(number_of_distinct_sexes > 1, 1, 0))]

  ## Select most recent sex
  Numapp <- numapp[numapp[, .I[cycle_year_month == max(cycle_year_month)], by=ssn]$V1]

  ## Select most recent sex
  numapp[,"sex_year_cycle" := year_cycle]
  numapp[,"sex_month_cycle" := month_cycle]


  numapp_sex <- numapp[, c("ssn", "sex", "sex_year_cycle", "sex_month_cycle", ), with=FALSE]

  return(numapp_sex)

}

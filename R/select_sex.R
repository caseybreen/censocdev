#' Select sex
#'
#' @param numapp path to the NUMAPP files
#' @return NUMAPP data.frame with best first name column and cycle dates
#' @keywords internal
#' @import data.table
#' @export

select_sex <- function(numapplication = numapp) {


  numapp <- numapp[, c("ssn", "sex", "cycle_date", "year_cycle", "month_cycle"), with=FALSE]

  # Clean & Create father_lname variable
  numapp[numapp==''|numapp==' ']<-NA
  numapp <- na.omit(numapp, cols="sex")
  numapp <- numapp[!(grepl("\\?", numapp$sex))]

  # Set missing values equal to 0. These will be selected last according to our selection process.
  for (col in c("year_cycle", "month_cycle")) numapp[is.na(get(col)), (col) := 0]

  # Maybe should convert this to century months in the future?
  numapp[,"cycle_year_month" := year_cycle + (month_cycle/12)]


  # For each SSN we want to select one row using the following rule:
  # The modal value of sex
  # If more than one mode, select the first value of sex.

  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }

  numapp <- numapp[numapp[, .I[sex==Mode(sex)], by=ssn]$V1]
  setkey(numapp, ssn, cycle_date)
  numapp_test <- numapp[J(unique(ssn)), mult = "first"]


  numapp[,"sex_year_cycle" := year_cycle]
  numapp[,"sex_month_cycle" := month_cycle]
  numapp_last_name <- numapp[, c("ssn", "sex", "sex_year_cycle", "sex_month_cycle"), with=FALSE]

  return(numapp_last_name)

}

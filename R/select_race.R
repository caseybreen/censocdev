#' Select race
#'
#' @param numapp path to the NUMAPP files
#' @return NUMAPP data.frame with best first name column and cycle dates
#' @keywords internal
#' @import data.table
#' @export

select_race <- function(numapplication = numapp) {


  numapp <- numapp[, c("ssn", "race", "cycle_date", "year_cycle", "month_cycle"), with=FALSE]

  # Clean & Create father_lname variable
  numapp[numapp==''|numapp==' '] <- NA
  numapp[numapp==0] <- NA
  numapp <- na.omit(numapp, cols="race")
  numapp <- numapp[!(grepl("\\?", numapp$race))]

  # Set missing values equal to 0. These will be selected last according to our selection process.
  for (col in c("year_cycle", "month_cycle")) numapp[is.na(get(col)), (col) := 0]

  # Maybe should convert this to century months in the future?
  numapp[,"cycle_year_month" := year_cycle + (month_cycle/12)]


  test <- numapp[, .(number_of_distinct_orders = uniqueN(race)), by = ssn]

  # For each SSN we want to select one row using the following rule:
  # The modal value of race
  # If more than one mode, select the first value of race.

  # Mode <- function(x) {
  #   ux <- unique(x)
  #   ux[which.max(tabulate(match(x, ux)))]
  # }

  numapp <- numapp[numapp[, .I[race==Mode(race)], by=ssn]$V1]
  setkey(numapp, ssn, cycle_date)
  numapp <- numapp[J(unique(ssn)), mult = "first"]


  numapp[,"race_year_cycle" := year_cycle]
  numapp[,"race_month_cycle" := month_cycle]
  numapp_race <- numapp[, c("ssn", "race", "race_year_cycle", "race_month_cycle"), with=FALSE]

  return(numapp_race)

}






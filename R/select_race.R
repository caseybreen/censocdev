#' Select race
#'
#' @param numapp path to the NUMAPP files
#' @return NUMAPP data.frame with best first name column and cycle dates
#' @keywords internal
#' @import data.table
#' @export

select_race <- function(numapplication = numapp) {


  numapp <- numapp[, c("ssn", "race", "cycle_date", "year_cycle", "month_cycle"), with=FALSE]

  ## Remove applications with 0 (no information) for sex
  applications <- nrow(numapp)
  numapp[race==0] <- NA
  numapp <- na.omit(numapp, cols="race")
  removed_na <- applications - nrow(numapp)
  cat(removed_na, "removed with 0 value (no information) or NA for race", "\n")

  # Set missing values equal to 0. These will be selected last according to our selection process.
  for (col in c("year_cycle", "month_cycle")) numapp[is.na(get(col)), (col) := 0]

  # Maybe should convert this to century months in the future?
  numapp[,"cycle_year_month" := year_cycle + (month_cycle/12)]

  ## Number of different sexes per SSN
  numapp[, number_of_distinct_races:=uniqueN(race), by = ssn]

  ## Create flag (0 or 1 dichotomous var) for more than one first name.
  numapp[, race_multiple_flag:=(ifelse(number_of_distinct_races > 1, 1, 0))]

  cat(removed_na, "Finished creating flag for multiple first names", "\n")

  ## Select most recent race
  numapp <- numapp[numapp[, .I[which.max(cycle_year_month)], by=ssn]$V1]

  ## Recode originally missing years back to NA.
  numapp[year_cycle == 0, year_cycle := NA]
  numapp[month_cycle == 0, month_cycle := NA]
  numapp[race == 9, race := NA]

  numapp[,"race_year_cycle" := year_cycle]
  numapp[,"race_month_cycle" := month_cycle]
  numapp_race <- numapp[, c("ssn", "race", "race_year_cycle", "race_month_cycle", "race_multiple_flag"), with=FALSE]

  return(numapp_race)

}




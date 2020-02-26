#' Select race
#'
#' @param numapp path to the NUMAPP files
#' @return data.frame with first race for persons with more than one race
#' @keywords internal
#' @import data.table
#' @export

select_race_first <- function(data = numapp) {

  data <- data[, c("ssn", "race", "cycle_date", "year_cycle", "month_cycle"), with=FALSE]

  ## Remove applications with 0 (no information) for sex
  applications <- nrow(data)
  data[race==0] <- NA
  data <- na.omit(data, cols="race")
  removed_na <- applications - nrow(data)
  cat(removed_na, "removed with 0 value (no information) or NA for race", "\n")

  # Set missing values equal to 0. These will be selected last according to our selection process.
  for (col in c("year_cycle", "month_cycle")) data[is.na(get(col)), (col) := 0]

  # Maybe should convert this to century months in the future?
  data[,"cycle_year_month" := year_cycle + (month_cycle/12)]

  ## Number of different apps per SSN
  data[, number_of_apps:=uniqueN(apps), by = ssn]

  data <- data[number_of_apps > 1]

  cat(removed_na, "Finished creating flag for multiple first names", "\n")

  ## Select most first race for persons with two or more apps
  data <- data[data[, .I[which.min(cycle_year_month)], by=ssn]$V1]

  ## Recode originally missing years back to NA.
  data[year_cycle == 0, year_cycle := NA]
  data[month_cycle == 0, month_cycle := NA]
  data[race == 9, race := NA]
  data[race_first := race]

  data[,"race_first_year" := year_cycle]
  data[,"race_first_month" := month_cycle]
  data.df <- data[, c("ssn", "race", "race_first_year", "race_first_month"), with=FALSE]

  return(data.df)

}




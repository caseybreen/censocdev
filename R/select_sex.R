#' Select sex
#'
#' @param numapp path to the NUMAPP files
#' @return NUMAPP data.frame with best first name column and cycle dates
#' @keywords internal
#' @import data.table
#' @export

select_sex <- function(data = numapp) {


  data <- data[, c("ssn", "sex", "cycle_date", "year_cycle", "month_cycle"), with=FALSE]

  ## Remove applications with 0 (no information) for sex
  applications <- nrow(data)
  data[sex==0] <- NA
  data[sex==" "] <- NA
  data <- na.omit(data, cols="sex")
  removed_na <- applications - nrow(data)
  cat(removed_na, "applications removed with 0 value (no information) or NA for sex", "\n")

  ## Set missing values equal to 0. These will be selected last according to our selection process.
  for (col in c("year_cycle", "month_cycle")) data[is.na(get(col)), (col) := 0]

  ## Maybe should convert this to century months in the future?
  data[,"cycle_year_month" := year_cycle + (month_cycle/12)]

  ## Number of different sexes per SSN
  data[, number_of_distinct_sexes:=uniqueN(sex), by = ssn]

  ## Create flag (0 or 1 dichotomous var) for more than one first name.
  data[, sex_multiple_flag:=(ifelse(number_of_distinct_sexes > 1, 1, 0))]

  ## Select most recent sex
  data <- data[data[, .I[which.max(cycle_year_month)], by=ssn]$V1]


  ## Select most recent sex
  data[,"sex_year_cycle" := year_cycle]
  data[,"sex_month_cycle" := month_cycle]


  ## Recode originally missing years back to NA.
  data[year_cycle == 0, year_cycle := NA]
  data[month_cycle == 0, month_cycle := NA]



  data.df <- data[, c("ssn", "sex", "sex_year_cycle", "sex_month_cycle", "sex_multiple_flag" ), with=FALSE]

  return(data.df)

}

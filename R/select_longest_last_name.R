#' Select best father last name
#'
#' @param numapp path to the NUMAPP files
#' @return NUMAPP data.frame with best first name column and cycle dates
#' @keywords internal
#' @import data.table
#' @export

select_longest_last_name <- function(numapp = numapp, variable = "father_fname") {

  ## select vars
  select.vars <- c("ssn", "cycle_date", "year_cycle", "month_cycle", variable)

  # Select variables from Num Application
  numapp <- numapp[, select.vars, with=FALSE]

  # Create & Clean variable
  numapp[,"name" := toupper(variable)]

  ## Remove applications with NA value for father_lname
  applications <- nrow(numapp)
  numapp <- na.omit(numapp, cols="name")
  removed_na <- applications - nrow(numapp)
  cat(removed_na, "removed with NA value for name", "\n")

  ## Remove applications with non-alphanumeric value for name
  applications <- nrow(numapp)
  numapp <- numapp[!(grepl("\\?", numapp$name))]
  removed_na <- applications - nrow(numapp)
  cat(removed_na, "removed with non-alphanumeric values for name", "\n")

  ## Remove applications with ZZZ values for name
  applications <- nrow(numapp)
  numapp <- numapp[!(grepl("ZZZ", numapp$name))]
  removed_na <- as.integer(applications - nrow(numapp))
  cat(removed_na, "removed with ZZZ values for name", "\n")

  ## Number of different first names per SSN
  numapp[, number_of_distinct_names:=uniqueN(name), by = ssn]

  ## Create flag (0 or 1 dichotomous var) for more than one first name.
  numapp[, paste0(variable, "_multiple_flag") := (ifelse(number_of_distinct_names > 1, 1, 0))]

  ## Select Longest First name (e.g. select "WILLIAM" over "BILL")
  numapp <- numapp[numapp[, .I[nchar(name) == max(nchar(name))], by = ssn]$V1]

  ## Maybe should convert this to century months in the future?
  numapp[,"cycle_year_month" := year_cycle + (month_cycle/12)]
  numapp$cycle_year_month[is.na(numapp$cycle_year_month)] <- 0

  ## Select most recent if there was a tie for longest name
  numapp <- numapp[numapp[, .I[which.max(cycle_year_month)], by=ssn]$V1]

  ## Recode originally missing years back to NA.
  numapp[year_cycle == 0, year_cycle := NA]
  numapp[month_cycle == 0, month_cycle := NA]

  ## Create father_lname year and cycle date vars
  numapp[,paste0(variable, "_year_cycle") := year_cycle]
  numapp[,paste0(variable, "_month_cycle") := month_cycle]

  ## Create data.table with specific data.table features
  numapp_father_last_name <- numapp[, c("ssn", variable, paste0(variable, "_year_cycle"), paste0(variable, "_month_cycle"), paste0(variable, "_multiple_flag")), with=FALSE]

  return(numapp_father_last_name)

}

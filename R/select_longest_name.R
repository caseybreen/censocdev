#' Select best name (e.g. best first name (fname), best last name (lname))
#'
#' @param numapp path to the NUMAPP files
#' @return NUMAPP data.frame with best first name column and cycle dates
#' @keywords internal
#' @import data.table
#' @export

select_longest_name <- function(data = numapp, name) {

  ## select vars
  select.vars <- c("ssn", "cycle_date", "year_cycle", "month_cycle", name)

  # Select variables from Num Application
  data <- data[, select.vars, with=FALSE]

  # Create & Clean variable
  data[,"name" := toupper(name)]

  ## Remove applications with NA value for name
  applications <- nrow(data)
  data <- na.omit(data, cols = name)
  removed_na <- applications - nrow(data)
  cat(removed_na, "removed with NA value for name", "\n")

  ## Remove applications with non-alphanumeric value for name
  applications <- nrow(data)
  data <- data[!(grepl("\\?", data$name))]
  removed_na <- applications - nrow(data)
  cat(removed_na, "removed with non-alphanumeric values for name", "\n")

  ## Remove applications with ZZZ values for name
  applications <- nrow(data)
  data <- data[!(grepl("ZZZ", data$name))]
  removed_na <- as.integer(applications - nrow(data))
  cat(removed_na, "removed with ZZZ values for name", "\n")

  ## Number of different first names per SSN
  data[, number_of_distinct_names:=uniqueN(name), by = ssn]

  ## Create flag (0 or 1 dichotomous var) for more than one first name.
  data[, paste0(name, "_multiple_flag") := (ifelse(number_of_distinct_names > 1, 1, 0))]

  ## Select Longest First name (e.g. select "WILLIAM" over "BILL")
  data <- data[data[, .I[nchar(name) == max(nchar(name))], by = ssn]$V1]

  ## Maybe should convert this to century months in the future?
  data[,"cycle_year_month" := year_cycle + (month_cycle/12)]
  data$cycle_year_month[is.na(data$cycle_year_month)] <- 0

  ## Select most recent if there was a tie for longest name
  data <- data[data[, .I[which.max(cycle_year_month)], by=ssn]$V1]

  ## Recode originally missing years back to NA.
  data[year_cycle == 0, year_cycle := NA]
  data[month_cycle == 0, month_cycle := NA]

  ## Create year and cycle date vars
  data[,paste0(name, "_year_cycle") := year_cycle]
  data[,paste0(name, "_month_cycle") := month_cycle]

  ## Create data.table with specific data.table features
  name.df <- data[, c("ssn", name, paste0(name, "_year_cycle"), paste0(name, "_month_cycle"), paste0(name, "_multiple_flag")), with=FALSE]

  return(name.df)

}

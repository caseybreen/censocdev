#' Select best first name
#'
#' @param data path to the NUMAPP files
#' @return numapp data.frame with best first name column
#' @keywords internal
#' @import data.table
#' @export

select_best_first_name <- function(data = numapp) {

  ## Select variables from Num Application
  data <- data[, c("ssn", "fname", "cycle_date", "year_cycle", "month_cycle"), with=FALSE]

  ## Create & Clean fname variable
  data[,"fname" := toupper(fname)]
  data[,"fname" := get_first_word(fname)]

  ## Remove applications with NA value for fname
  applications <- nrow(data)
  data <- na.omit(data, cols="fname")
  removed_na <- applications - nrow(data)
  cat(removed_na, "removed with NA value for fname", "\n")

  ## Remove applications with non-alphanumeric value for fname
  applications <- nrow(data)
  data <- data[!(grepl("\\?", data$fname))]
  removed_na <- applications - nrow(data)
  cat(removed_na, "removed with non-alphanumeric values for fnames", "\n")

  ## Remove applications with ZZZ values for fname
  applications <- nrow(data)
  data <- data[!(grepl("ZZZ", data$fname))]
  removed_na <- as.integer(applications - nrow(data))
  cat(removed_na, "removed with ZZZ values for fname", "\n")

  ## Number of different first names per SSN
  data[, number_of_distinct_names:=data.table::uniqueN(fname), by = ssn]

  ## Create flag (0 or 1 dichotomous var) for more than one first name.
  data[, fname_multiple_flag:=(ifelse(number_of_distinct_names > 1, 1, 0))]

  ## Select Longest First name (e.g. select "WILLIAM" over "BILL")
  data <- data[data[, .I[nchar(fname) == max(nchar(fname))], by = ssn]$V1]

  ## Maybe should convert this to century months in the future?
  data[,"cycle_year_month" := year_cycle + (month_cycle/12)]
  data$cycle_year_month[is.na(data$cycle_year_month)] <- 0

  ## Select most recent if there was a tie for longest name
  data <- data[data[, .I[which.max(cycle_year_month)], by=ssn]$V1]

  data[,"fname_year_cycle" := year_cycle]
  data[,"fname_month_cycle" := month_cycle]
  data.df <- data[, c("ssn", "fname", "fname_year_cycle", "fname_month_cycle", "fname_multiple_flag"), with=FALSE]

  return(data.df)
}

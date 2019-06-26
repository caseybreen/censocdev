#' Select best first name
#'
#' @param numapp path to the NUMAPP files
#' @return numapp data.frame with best first name column
#' @keywords internal
#' @import data.table
#' @export

select_best_first_name <- function(numapplication = numapp) {
  
  ## Select variables from Num Application
  numapp <- numapplication[, c("ssn", "nh_name_first", "cycle_date", "year_cycle", "month_cycle"), with=FALSE]
  
  ## Create & Clean fname variable
  numapp[,"fname" := toupper(nh_name_first)]
  numapp[,"fname" := get_first_word(fname)]
  
  ## Remove applications with NA value for fname
  applications <- nrow(numapp)
  numapp <- na.omit(numapp, cols="fname")
  removed_na <- applications - nrow(numapp)
  cat(removed_na, "removed with NA value for fname", "\n")
  
  ## Remove applications with non-alphanumeric value for fname
  applications <- nrow(numapp)
  numapp <- numapp[!(grepl("\\?", numapp$fname))]
  removed_na <- applications - nrow(numapp)
  cat(removed_na, "removed with non-alphanumeric values for fnames", "\n")
  
  ## Remove applications with ZZZ values for fname
  applications <- nrow(numapp)
  numapp <- numapp[!(grepl("ZZZ", numapp$fname))]
  removed_na <- as.integer(applications - nrow(numapp))
  cat(removed_na, "removed with ZZZ values for fname", "\n")
  
  ## Number of different first names per SSN
  numapp[, number_of_distinct_names:=data.table::uniqueN(fname), by = ssn]
  
  ## Create flag (0 or 1 dichotomous var) for more than one first name.
  numapp[, fname_multiple_flag:=(ifelse(number_of_distinct_names > 1, 1, 0))]
  
  ## Select Longest First name (e.g. select "WILLIAM" over "BILL")
  numapp <- numapp[numapp[, .I[nchar(fname) == max(nchar(fname))], by = ssn]$V1]
  
  ## Maybe should convert this to century months in the future?
  numapp[,"cycle_year_month" := year_cycle + (month_cycle/12)]
  numapp$cycle_year_month[is.na(numapp$cycle_year_month)] <- 0
  
  ## Select most recent if there was a tie for longest name
  numapp <- numapp[numapp[, .I[which.max(cycle_year_month)], by=ssn]$V1]
  
  numapp[,"fname_year_cycle" := year_cycle]
  numapp[,"fname_month_cycle" := month_cycle]
  numapp_first_name <- numapp[, c("ssn", "fname", "fname_year_cycle", "fname_month_cycle", "fname_multiple_flag"), with=FALSE]
  
  return(numapp_first_name)
}

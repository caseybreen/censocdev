#' Select best last name
#'
#' @param numapp path to the NUMAPP files
#' @return numapp data.frame with best last name column
#' @keywords internal
#' @import data.table
#' @export

select_best_last_name <- function(numapp = numapp) {
  
  ## Select variables from Num Application
  numapp <- numapp[, c("ssn", "nh_name_last", "cycle_date", "year_cycle", "month_cycle"), with=FALSE]
  
  ## Create & Clean lname variable
  numapp[,"lname" := toupper(nh_name_last)]
  numapp[,"lname" := get_first_word(lname)]
  
  ## Remove applications with NA value for lname
  applications <- nrow(numapp)
  numapp <- na.omit(numapp, cols="lname")
  removed_na <- applications - nrow(numapp)
  cat(removed_na, "removed with NA value for lname", "\n")
  
  ## Remove applications with non-alphanumeric value for lname
  applications <- nrow(numapp)
  numapp <- numapp[!(grepl("\\?", numapp$lname))]
  removed_na <- applications - nrow(numapp)
  cat(removed_na, "removed with non-alphanumeric values for lnames", "\n")
  
  ## Remove applications with ZZZ values for lname
  applications <- nrow(numapp)
  numapp <- numapp[!(grepl("ZZZ", numapp$lname))]
  removed_na <- as.integer(applications - nrow(numapp))
  cat(removed_na, "removed with ZZZ values for lname", "\n")
  
  ## Number of different last names per SSN
  numapp[, number_of_distinct_names:=data.table::uniqueN(lname), by = ssn]
  
  ## Create flag (0 or 1 dichotomous var) for more than one last name.
  numapp[, lname_multiple_flag:=(ifelse(number_of_distinct_names > 1, 1, 0))]
  
  ## Select Longest Last name (e.g. select "WILLIAM" over "BILL")
  numapp <- numapp[numapp[, .I[nchar(lname) == max(nchar(lname))], by = ssn]$V1]
  
  ## Maybe should convert this to century months in the future?
  numapp[,"cycle_year_month" := year_cycle + (month_cycle/12)]
  numapp$cycle_year_month[is.na(numapp$cycle_year_month)] <- 0
  
  ## Select most recent if there was a tie for longest name
  numapp <- numapp[numapp[, .I[which.max(cycle_year_month)], by=ssn]$V1]

  numapp[,"lname_year_cycle" := year_cycle]
  numapp[,"lname_month_cycle" := month_cycle]
  numapp_middle_name <- numapp[, c("ssn", "lname", "lname_year_cycle", "lname_month_cycle", "lname_multiple_flag"), with=FALSE]
  
}

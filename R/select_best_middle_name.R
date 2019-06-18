#' Select best last name
#'
#' @param numapp path to the NUMAPP files
#' @return numapp data.frame with best middle name column
#' @keywords internal
#' @import data.table
#' @export

select_best_middle_name <- function(numapplication = numapp) {

  ## Select variables from Num Application
  numapp <- numapplication[, c("ssn", "nh_name_middle", "cycle_date", "year_cycle", "month_cycle"), with=FALSE]

  ## Create & Clean mname variable
  numapp[,"mname" := toupper(nh_name_middle)]
  numapp[,"mname" := get_first_word(mname)]

  ## Remove applications with NA value for mname
  applications <- nrow(numapp)
  numapp <- na.omit(numapp, cols="mname")
  removed_na <- applications - nrow(numapp)
  cat(removed_na, "removed with NA value for mname", "\n")

  ## Remove applications with non-alphanumeric value for mname
  applications <- nrow(numapp)
  numapp <- numapp[!(grepl("\\?", numapp$mname))]
  removed_na <- applications - nrow(numapp)
  cat(removed_na, "removed with non-alphanumeric values for mname", "\n")

  ## Remove applications with ZZZ values for mname
  applications <- nrow(numapp)
  numapp <- numapp[!(grepl("ZZZ", numapp$mname))]
  removed_na <- as.integer(applications - nrow(numapp))
  cat(removed_na, "removed with ZZZ values for mname", "\n")
  
  ## Number of different first names per SSN
  numapp[, number_of_distinct_names:=uniqueN(mname), by = ssn]
  
  ## Create flag (0 or 1 dichotomous var) for more than one first name.
  numapp[, mname_multiple_flag:=(ifelse(number_of_distinct_names > 1, 1, 0))]
  
  ## Select Longest First name (e.g. select "WILLIAM" over "BILL")
  numapp <- numapp[numapp[, .I[nchar(mname) == max(nchar(mname))], by = ssn]$V1]
  
  ## Maybe should convert this to century months in the future?
  numapp[,"cycle_year_month" := year_cycle + (month_cycle/12)]
  
  ## Select most recent if there was a tie for longest name
  numapp <- numapp[numapp[, .I[which.max(cycle_year_month)], by=ssn]$V1]
  
  ## Recode originally missing years back to NA.
  numapp[year_cycle == 0, year_cycle := NA]
  numapp[month_cycle == 0, month_cycle := NA]
  
  numapp[,"mname_year_cycle" := year_cycle]
  numapp[,"mname_month_cycle" := month_cycle]
  numapp_middle_name <- numapp[, c("ssn", "mname", "mname_year_cycle", "mname_month_cycle", "mname_multiple_flag"), with=FALSE]

}

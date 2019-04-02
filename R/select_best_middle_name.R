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
  numapp[, number_of_distinct_last_names:=uniqueN(mname), by = ssn]

  ## Create flag (0 or 1 dichotomous var) for more than one first name.
  numapp[, mname_multiple_flag:=(ifelse(number_of_distinct_last_names > 1, 1, 0))]

  ## Set missing values equal to 0. These will be selected last according to our selection process.
  for (col in c("year_cycle", "month_cycle")) numapp[is.na(get(col)), (col) := 0]

  ## Maybe should convert this to century months in the future?
  numapp[,"cycle_year_month" := year_cycle + (month_cycle/12)]


  ## For each SSN we want to select one row using the following rule:
  ## If there is a year > 1940, select the row with minimum year above 1940
  ## If there not a year > 1940, select the row with the maximum year.
  numapp <- numapp[
    numapp[
      ,
      if(any(cycle_year_month > 1940))
        .I[which.min(1940 - cycle_year_month)] else
          .I[which.max(cycle_year_month)],
      by=ssn
      ]$V1
    ]

  ## Recode originally missing years back to NA.
  numapp[year_cycle == 0, year_cycle := NA]
  numapp[month_cycle == 0, month_cycle := NA]

  ## Create cycle date variables
  numapp[,"mname_year_cycle" := year_cycle]
  numapp[,"mname_month_cycle" := month_cycle]
  numapp_middle_name <- numapp[, c("ssn", "mname", "mname_year_cycle", "mname_month_cycle", "mname_multiple_flag"), with=FALSE]


  ## Recode originally missing years back to NA.
  numapp[year_cycle == 0, year_cycle := NA]
  numapp[month_cycle == 0, month_cycle := NA]


  return(numapp_middle_name)

}

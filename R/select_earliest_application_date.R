#' Date of first application
#'
#' @param numapp path to the NUMAPP files
#' @return NUMAPP data.frame with number of applications
#' @keywords internal
#' @import data.table
#' @export

select_date_of_first_app <- function(numapp = numapp) {

  ## calculate month and year from cycle date
  numapp[, "earliest_cycle_year" := as.numeric(substr(cycle_date, 1, 4))]
  numapp[, "earliest_cycle_month" := as.numeric(substr(cycle_date, 5, 6))]

  ## drop all cases where earliest_cycle_year is NA.
  applications <- nrow(numapp)
  numapp <- na.omit(numapp, cols="earliest_cycle_year")
  removed_na <- applications - nrow(numapp)
  cat(removed_na, "removed with NA value for earliest_cycle_year", "\n")

  ## If month is missing, let's recode to 6.
  numapp[, earliest_cycle_month := ifelse(is.na(earliest_cycle_month), 6, earliest_cycle_month)]

  ## select the minimum cycle year
  ## keep all records with a tie
  numapp <- numapp[numapp[, .I[earliest_cycle_year == min(earliest_cycle_year)], by = ssn]$V1]

  ## select minimum cycle month
  ## if tie, select only first row
  numapp <- numapp[numapp[, .I[which.max(earliest_cycle_month)], by = ssn]$V1]

  numapp <- numapp[, .(ssn, earliest_cycle_month, earliest_cycle_year)]

  return(numapp)
}


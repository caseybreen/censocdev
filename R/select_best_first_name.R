#' Get first word from a string
#'
#' @param numapp path to the NUMAPP files
#' @return NUMAPP data.frame with best first name column and cycle dates
#' @keywords internal
#' @import data.table
#' @export

select_best_first_name <- function(numapplication = numapp) {

  numapplication <- numapp[, list(unique(ssn))]

  # Clean & Create fname variable

  numapp[,"fname" := toupper(nh_name_first)]

  numapp <- numapp[, c("ssn", "fname", "cycle_date", "year_cycle", "month_cycle"), with=FALSE]

  numapp[,"fname" := get_first_word(fname)]
  numapp <- na.omit(numapp, cols="fname")
  numapp <- numapp[!(grepl("\\?", numapp$fname))]
  numapp <- numapp[!(grepl("ZZZ", numapp$fname))]


  # Set missing values equal to 0. These will be selected last according to our selection process.
  for (col in c("year_cycle", "month_cycle")) numapp[is.na(get(col)), (col) := 0]

  # Maybe should convert this to century months in the future?
  numapp[,"cycle_year_month" := year_cycle + (month_cycle/12)]


  # For each SSN we want to select one row using the following rule:
  # If there is a year > 2000, select the row with minimum year above 2000.
  # If there not a year > 2000, select the row with the maximum year.
  numapp <- numapp[
    numapp[
      ,
      if(any(cycle_year_month > 1940))
        .I[which.min(1940 - cycle_year_month)] else
          .I[which.max(cycle_year_month)],
      by=ssn
      ]$V1
    ]

  numapp[,"fname_year_cycle" := year_cycle]
  numapp[,"fname_month_cycle" := month_cycle]
  numapp_last_name <- numapp[, c("ssn", "fname", "fname_year_cycle", "fname_month_cycle"), with=FALSE]

  return(numapp_last_name)

}

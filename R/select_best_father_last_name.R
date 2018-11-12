#' Select best father last name
#'
#' @param numapp path to the NUMAPP files
#' @return NUMAPP data.frame with best first name column and cycle dates
#' @keywords internal
#' @import data.table
#' @export

select_best_first_name <- function(numapplication = numapp) {

  # Select variables from Num Application
  numapp <- numapp[, c("ssn", "father_lname", "cycle_date", "year_cycle", "month_cycle"), with=FALSE]

  # Create & Clean father_lname variable
  numapp[,"father_lname" := toupper(fth_name_last)]


  numapp[,"father_lname" := get_first_word(father_lname)]
  numapp <- na.omit(numapp, cols="father_lname")
  numapp <- numapp[!(grepl("\\?", numapp$father_lname))]
  numapp <- numapp[!(grepl("ZZZ", numapp$father_lname))]


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

  numapp[,"father_lname_year_cycle" := year_cycle]
  numapp[,"father_lname_month_cycle" := month_cycle]
  numapp_last_name <- numapp[, c("ssn", "father_lname", "father_lname_year_cycle", "father_lname_month_cycle"), with=FALSE]

  return(numapp_last_name)

}

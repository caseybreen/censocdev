#' Select best last name
#'
#' @param numapp path to the NUMAPP files
#' @return numapp data.frame with best last name column
#' @keywords internal
#' @import data.table
#' @export

  select_best_last_name <- function(numapplication = numapp) {

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
    cat(removed_na, "removed with non-alphanumeric values for lname", "\n")

    ## Remove applications with ZZZ values for lname
    applications <- nrow(numapp)
    numapp <- numapp[!(grepl("ZZZ", numapp$lname))]
    removed_na <- as.integer(applications - nrow(numapp))
    cat(removed_na, "removed with ZZZ values for lname", "\n")

    ## Number of different first names per SSN
    numapp[, number_of_distinct_last_names:=uniqueN(lname), by = ssn]

    ## Create flag (0 or 1 dichotomous var) for more than one first name.
    numapp[, lname_multiple_flag:=(ifelse(number_of_distinct_last_names > 1, 1, 0))]

    ## Set missing values equal to 0. These will be selected last according to our selection process.
    for (col in c("year_cycle", "month_cycle")) numapp[is.na(get(col)), (col) := 0]

    ## Maybe should convert this to century months in the future?
    numapp[,"cycle_year_month" := year_cycle + (month_cycle/12)]


    ## For each SSN we want to select one row using the following rule:
    ## If there is a year > 2000, select the row with minimum year above 2000.
    ## If there not a year > 2000, select the row with the maximum year.
    numapp <- numapp[
      numapp[
        ,
        if(any(cycle_year_month > 1940))
          .I[which.min(1940 - cycle_year_month)] else
            .I[which.max(cycle_year_month)],
        by=ssn
        ]$V1
      ]

    numapp[,"lname_year_cycle" := year_cycle]
    numapp[,"lname_month_cycle" := month_cycle]
    numapp_last_name <- numapp[, c("ssn", "lname", "lname_year_cycle", "lname_month_cycle", "lname_multiple_flag"), with=FALSE]


    ## Recode originally missing years back to NA.
    numapp[year_cycle == 0, year_cycle := NA]
    numapp[month_cycle == 0, month_cycle := NA]


    return(numapp_last_name)

}

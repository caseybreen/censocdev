#' Get first word from a string
#'
#' @param numapp_path path to the NUMAPP files
#' @return NUMAPP data.frame with best last name column
#' @keywords internal
#' @import data.table
#' @export

  select_best_last_name <- function(numapplication = numapp) {

    numapplication <- numapp[, list(unique(ssn))]

    # Clean & Create lname variable

    numapp[,"lname" := toupper(nh_name_last)]

    numapp <- numapp[, c("ssn", "lname", "cycle_date"), with=FALSE]

    numapp[,"lname" := get_first_word(lname)]
    numapp <- na.omit(numapp, cols="lname")

    numapp <- numapp[!(grepl("\\?", numapp$lname))]

    numapp <- numapp[!(grepl("ZZZ", numapp$lname))]
    test2 <- numapp[, .(number_of_distinct_orders = uniqueN(lname)), by = ssn]

}

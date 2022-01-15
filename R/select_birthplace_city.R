#' Select select Birthplace City
#'
#' @param numapp path to the NUMAPP files
#' @return NUMAPP data.frame with best bpl column and cycle dates
#' @keywords internal
#' @import data.table
#' @export

select_birthplace_city <- function(data = numapp) {


  data <- data[, c("ssn", "pob_city","pob_city_oflo", "year_cycle", "month_cycle"), with=FALSE]


  ## Remove applications with 0 (no information) for BPL
  applications <- nrow(data)
  data[pob_city == " "] <- NA
  data <- na.omit(data, cols="pob_city")
  removed_na <- applications - nrow(data)
  cat(removed_na, "applications removed with NA for bpl", "\n")

  ## Set missing values equal to 0. These will be selected last according to our selection process.
  for (col in c("year_cycle", "month_cycle")) data[is.na(get(col)), (col) := 0]

  ## Maybe should convert this to century months in the future?
  data[,"cycle_year_month" := year_cycle + (month_cycle/12)]
  data$cycle_year_month[is.na(data$cycle_year_month)] <- 0

  ## Number of different BPL per SSN
  data[, number_of_distinct_bpl:=uniqueN(pob_city), by = ssn]

  ## Create flag (0 or 1 dichotomous var) for more than one birth place
  data[, bpl_city_multiple_flag := (ifelse(number_of_distinct_bpl > 1, 1, 0))]

  ## Select most recent BPL
  data <- data[data[, .I[which.max(cycle_year_month)], by=ssn]$V1]

  ## Recode BPL
  data[,"bpl_city_year_cycle" := year_cycle]
  data[,"bpl_city_month_cycle" := month_cycle]

  ## Recode originally missing years back to NA.
  data[bpl_city_year_cycle == 0, bpl_city_year_cycle := NA]
  data[bpl_city_month_cycle == 0, bpl_city_month_cycle := NA]

  numapp_bpl <- data[, c("ssn", "pob_city", "pob_city_oflo", "bpl_city_year_cycle", "bpl_city_month_cycle", "bpl_city_multiple_flag" ), with=FALSE]

  return(numapp_bpl)

}

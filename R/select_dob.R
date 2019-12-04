#' Select best dob
#'
#' @param numapp path to the NUMAPP files
#' @return NUMAPP data.frame with best dob column for each unique ssn (40870456) and cycle dates
#' @keywords internal
#' @import data.table
#' @export
#'
select_dob <- function(data = numapp) {

  applications <- nrow(data)

  data[, "dob_cyear" := as.numeric(substr(cycle_date, 1, 4))]
  data[, "dob_cmonth" := as.numeric(substr(cycle_date, 5, 6))]

  # Select variables from Num Application
  data <- data[ , .(ssn, dob, dob_cmonth, dob_cyear)]

  ## Keeping only differnts pairs of ssn and first names.
  data = data[!duplicated(data, by=c("ssn","dob"))]

  ## Remove applications with NA value
  applications <- nrow(data)
  data <- na.omit(data, cols="dob")
  removed_na <- applications - nrow(data)
  cat(removed_na, "removed with NA value for dob", "\n")

  ## Remove applications with non-alphanumeric value
  applications <- nrow(data)
  data <- data[!(grepl("\\?", data$dob))]
  removed_na <- applications - nrow(data)
  cat(removed_na, "removed with non-alphanumeric values", "\n")

  ## Remove applications with ZZZ values
  applications <- nrow(data)
  data <- data[!(grepl("ZZZ", data$dob))]
  removed_na <- as.integer(applications - nrow(data))
  cat(removed_na, "removed with non-alphanumeric values", "\n")

  ## Number of different dob per SSN
  data[, number_of_distinct_dob:=uniqueN(dob), by = ssn]

  ## Recode dob values of 0 to NA
  data[ , dob:= (ifelse(dob=="0", NA, dob)) ]


  ## Create flag (0 or 1 dichotomous var) for more than one dob.
  data[, dob_multiple_flag:=(ifelse(number_of_distinct_dob > 1, 1, 0))]

  ## Keep a frame only with duplicates.
  app_dob_dupli = data[dob_multiple_flag==1, ]
  ## Keep a frame only with uniques
  app_dob_unique = data[dob_multiple_flag==0, ]

  #Duplicates with NAs
  app_dob_dupli[, cyear_month := dob_cyear * 100 +  dob_cmonth]
  app_dob_dupli[, cycle_na:= is.na(cyear_month)]
  nrow(app_dob_dupli[ cycle_na==TRUE,])
  #223,463 it will be random for them based on recency.

  ##Recency
  app_dob_dupli[, ssn_n := order(dob_cyear, dob_cmonth), by  = c("ssn")]
  app_dob_dupli[, latest_record:= .(max(ssn_n)), by=c("ssn") ]

  app_dob_dupli = app_dob_dupli[(latest_record==ssn_n), ]

  nrow(app_dob_dupli[!duplicated(app_dob_dupli, by=c("ssn"))])

  app_dob_dupli = app_dob_dupli[, .(ssn, dob, dob_cyear, dob_cmonth, dob_multiple_flag)]
  app_dob_unique = app_dob_unique[, .(ssn, dob, dob_cyear, dob_cmonth, dob_multiple_flag)]
  numapp_dob = rbind(app_dob_unique,app_dob_dupli)

  ## Create byear, bmonth, and bday variables.
  numapp_dob[, "bmonth" := as.numeric(substr(dob, 1, 2))]

  ## 5 cases where bmonth is 0; replace with NA
  numapp_dob[, bmonth:= (ifelse(bmonth=="0", NA, bmonth)) ]

  numapp_dob[, "bday" := as.numeric(substr(dob, 3, 4))]

  ## 34 cases where bmonth is 0; replace with NA
  numapp_dob[, bday:= (ifelse(bday=="0", NA, bday))]

  numapp_dob[, "byear" := as.numeric(substr(dob, 5, 8))]

  numapp_dob <- numapp_dob[, .(ssn, byear, bmonth, bday, dob_cyear, dob_cmonth, dob_multiple_flag)]

  return(numapp_dob)
}

#' Select best dob
#'
#' @param numapp path to the NUMAPP files
#' @return NUMAPP data.frame with best dob column for each unique ssn (40870456) and cycle dates
#' @keywords internal
#' @import data.table
#' @export
#'
select_dob <- function(numapp = numapp) {

  applications <- nrow(numapp)

  numapp[, "dob_cyear" := as.numeric(substr(cycle_date, 1, 4))]
  numapp[, "dob_cmonth" := as.numeric(substr(cycle_date, 5, 6))]

  # Select variables from Num Application
  numapp <- numapp[ , .(ssn, dob, dob_cmonth, dob_cyear)]
  ## Keeping only differnts pairs of ssn and first names.
  numapp = numapp[!duplicated(numapp, by=c("ssn","dob"))]

  ## Remove applications with NA value
  applications <- nrow(numapp)
  numapp <- na.omit(numapp, cols="dob")
  removed_na <- applications - nrow(numapp)
  cat(removed_na, "removed with NA value for dob", "\n")

  ## Remove applications with non-alphanumeric value
  applications <- nrow(numapp)
  numapp <- numapp[!(grepl("\\?", numapp$dob))]
  removed_na <- applications - nrow(numapp)
  cat(removed_na, "removed with non-alphanumeric values", "\n")

  ## Remove applications with ZZZ values
  applications <- nrow(numapp)
  numapp <- numapp[!(grepl("ZZZ", numapp$dob))]
  removed_na <- as.integer(applications - nrow(numapp))
  cat(removed_na, "removed with non-alphanumeric values", "\n")

  ## Number of different dob per SSN
  numapp[, number_of_distinct_dob:=uniqueN(dob), by = ssn]

  ## Create flag (0 or 1 dichotomous var) for more than one dob.
  numapp[, dob_multiple_flag:=(ifelse(number_of_distinct_dob > 1, 1, 0))]

  ## Keep a frame only with duplicates.
  app_dob_dupli = numapp[dob_multiple_flag==1, ]
  ## Keep a frame only with uniques
  app_dob_unique = numapp[dob_multiple_flag==0, ]

  #number of unique records among the duplicates
  #nrow(app_dob_dupli[ssn_n == 1, ])
  #2,512,336

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
  numapp = rbind(app_dob_unique,app_dob_dupli)
  rm(app_dob_dupli)
  rm(app_dob_unique)
  return(numapp_dob)
}

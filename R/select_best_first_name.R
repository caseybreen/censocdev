#' Select best first name
#'
#' @param numapp path to the NUMAPP files
#' @return NUMAPP data.frame with best first name column for each unique ssn (40870456) and cycle dates
#' @keywords internal
#' @import data.table
#' @export
#'
select_best_first_name <- function(numapp = numapp) {

  applications <- nrow(numapp)

  numapp[, "fname_cyear" := as.numeric(substr(cycle_date, 1, 4))]
  numapp[, "fname_cmonth" := as.numeric(substr(cycle_date, 5, 6))]
  numapp[,"fname" := toupper(nh_name_first)]
  numapp[,"fname" := get_first_word(fname)]

  # Select variables from Num Application
  numapp <- numapp[ , .(ssn, fname, fname_cmonth, fname_cyear)]
  ## Keeping only differnts pairs of ssn and first names.
  numapp = numapp[!duplicated(numapp, by=c("ssn","fname"))]

  ## Remove applications with NA value for fname
  applications <- nrow(numapp)
  numapp <- na.omit(numapp, cols="fname")
  removed_na <- applications - nrow(numapp)
  cat(removed_na, "removed with NA value for fname", "\n")

  ## Remove applications with non-alphanumeric value for fname
  applications <- nrow(numapp)
  numapp <- numapp[!(grepl("\\?", numapp$fname))]
  removed_na <- applications - nrow(numapp)
  cat(removed_na, "removed with non-alphanumeric values for fname", "\n")

  ## Remove applications with ZZZ values for fname
  applications <- nrow(numapp)
  numapp <- numapp[!(grepl("ZZZ", numapp$fname))]
  removed_na <- as.integer(applications - nrow(numapp))
  cat(removed_na, "removed with non-alphanumeric values for fname", "\n")

  ## Number of different first names per SSN
  numapp[, number_of_distinct_names:=uniqueN(fname), by = ssn]

  ## Create flag (0 or 1 dichotomous var) for more than one first name.
  numapp[, fname_multiple_flag:=(ifelse(number_of_distinct_names > 1, 1, 0))]

  ## Keep a frame only with duplicates.
  app_fname_dupli = numapp[fname_multiple_flag==1, ]
  #5,166,368
  #keep a frame only with uniques
  app_fname_unique = numapp[fname_multiple_flag==0, ]

  #number of unique records among the duplicates
  #nrow(app_fname_dupli[ssn_n == 1, ])
  #2,512,336

  ## Select Longest First name (e.g. select "WILLIAM" over "BILL")
  #numapp <- numapp[numapp[, .I[which.max(nchar(fname))], by = ssn]$V1]
  app_fname_dupli[, nchar_fname := nchar(fname), by  = c("ssn")]
  app_fname_dupli[, longest_fname:= .(max(nchar_fname)), by=c("ssn") ]
  app_fname_dupli = app_fname_dupli[longest_fname==nchar_fname, ]

  app_fname_dupli[, cyear_month := fname_cyear * 100 +  fname_cmonth]

  #Duplicates with NAs
  app_fname_dupli[, cycle_na:= is.na(cyear_month)]
  nrow(app_fname_dupli[ cycle_na==TRUE,])
  #223,463 it will be random for them based on recency.

  ##Recency
  app_fname_dupli[, ssn_n := order(fname_cyear, fname_cmonth), by  = c("ssn")]
  app_fname_dupli[, latest_record:= .(max(ssn_n)), by=c("ssn") ]

  app_fname_dupli = app_fname_dupli[(latest_record==ssn_n), ]

  app_fname_dupli = app_fname_dupli[, .(ssn, fname, fname_cyear, fname_cmonth, fname_multiple_flag)]
  app_fname_unique = app_fname_unique[, .(ssn, fname,fname_cyear, fname_cmonth, fname_multiple_flag)]
  numapp = rbind(app_fname_unique,app_fname_dupli)
  rm(app_fname_dupli)
  rm(app_fname_unique)
  return(numapp)
}

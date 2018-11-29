#' Select race
#'
#' @param numapp path to the NUMAPP files
#' @return NUMAPP data.frame with best first name column and cycle dates
#' @keywords internal
#' @import data.table
#' @export

select_race <- function(numapplication = numapp) {

  numapp[, "race_cyear" := as.numeric(substr(cycle_date, 1, 4))]
  numapp[, "race_cmonth" := as.numeric(substr(cycle_date, 5, 6))]

    # Select variables from Num Application
  numapp <- numapp[ , .(ssn, race, race_cmonth, race_cyear)]

  ## Keeping only duplicates with equal ssn but different race
  numapp = numapp[!duplicated(numapp, by=c("ssn","race"))]

  #Identifying 0 as NA
  numapp[, race := as.numeric(race)]
  numapp[is.na(race), race := 0]

  ## Remove applications with 0 (no information) for race
  #applications <- nrow(numapp)
  #numapp[race==0] <- NA
  #numapp <- na.omit(numapp, cols="race")
  #removed_na <- applications - nrow(numapp)
  #cat(removed_na, "removed with 0 value (no information) or NA for race", "\n")

  ## Remove applications with non-alphanumeric value for fname
  applications <- nrow(numapp)
  numapp <- numapp[!(grepl("\\?", numapp$race))]
  removed_na <- applications - nrow(numapp)
  cat(removed_na, "removed with non-alphanumeric values for fname", "\n")

  ## Remove applications with ZZZ values for fname
  applications <- nrow(numapp)
  numapp <- numapp[!(grepl("ZZZ", numapp$race))]
  removed_na <- as.integer(applications - nrow(numapp))
  cat(removed_na, "removed with non-alphanumeric values for fname", "\n")

  # Set missing values equal to 0. These will be selected last according to our selection process.
  #for (col in c("race_cyear", "race_cmonth")) numapp[is.na(get(col)), (col) := 0]

  # Maybe should convert this to century months in the future?
  #numapp[,"cycle_year_month" := year_cycle + (month_cycle/12)]

  # For each SSN we want to select one row using the following rule:
  # The modal value of race
  # If more than one mode, select the first value of race.

  # mode function.
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }

  ## Number of different races per SSN
  numapp[, number_of_distinct_races:=uniqueN(race), by = ssn]

  ## Create flag (0 or 1 dichotomous var) for more than one first name.
  numapp[, race_multiple_flag:=(ifelse(number_of_distinct_races > 1, 1, 0))]

  ## Keep a frame only with duplicates.
  app_race_dupli = numapp[race_multiple_flag==1, ]
  #5,166,368
  #keep a frame only with uniques
  app_race_unique = numapp[race_multiple_flag==0, ]

  ## Keep the mode of the race.
  app_race_dupli[, race_min:= .(min(race)), by=c("ssn") ]
  app_race_dupli[, race_max:= .(max(race)), by=c("ssn") ]
  #keeping only duplicates among race distinct than 0, this means
  #keeping only duplicates at 0 for all the records.
  app_race_dupli=app_race_dupli[race > 0 | race_max == 0, ]
  #6,221,017
  table(app_race_dupli$race)
  #0       1       2       3       4       5       6       9
  #1136 4126165  882923  397017  158329  569361   84215    1871

    ## Mode
  app_race_dupli[, race_mode:= .(getmode(race)), by=c("ssn") ]
  app_race_dupli = app_race_dupli[race_mode== race,]

  ## Select most recent race
  #numapp <- numapp[numapp[, .I[which.max(cycle_year_month)], by=ssn]$V1]

  ## Recode originally missing years back to NA.
  #numapp[year_cycle == 0, year_cycle := NA]
  #numapp[month_cycle == 0, month_cycle := NA]

  app_race_dupli = app_race_dupli[, .(ssn, race,race_cyear, race_cmonth, race_multiple_flag)]
  app_race_unique = app_race_unique[, .(ssn, race,race_cyear, race_cmonth, race_multiple_flag)]
  numapp = rbind(app_race_unique,app_race_dupli)
  rm(app_race_dupli)
  rm(app_race_unique)

    return(numapp_race)

}






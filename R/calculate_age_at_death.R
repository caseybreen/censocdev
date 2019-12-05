#' Calculate age at Death
#'
#' @param data data.frame with birth and death info
#' @return data.frame
#' @keywords internal
#' @import data.table
#' @export
#'

calculate_age_at_death <- function(data) {
  setDT(data)
  ######## age_at_death

  ## This is a function written by Josh

  ## Only cases:
  ## (1) birthday before (or equal to) death day --> dyear - byear
  ## * dmonth > bmonth
  ## * dmonth == bmonth & dday >= bday

  ## (2) birthday after death day --> dyear - byear - 1
  ## * dmonth < bmonth
  ## * dmonth == bmonth & dday < bday

  ## BUT we can have  missing values on
  ## bday, bmonth, byear, dday, dmonth, and dyear

  ## (1) byear or dyear missing --> age_at_death = missing
  ## (2) bmonth only missing  --> impute as June
  ## (3) dmonth only missing  --> impute as June
  ## (4) dday only missing  --> impute as 15
  ## (5) bday only missing  --> impute as 15
  data[, bmonth_hat := bmonth]
  data[is.na(bmonth), bmonth_hat := 6]
  data[, dmonth_hat := dmonth]
  data[is.na(dmonth), dmonth_hat := 6]
  ##
  data[, bday_hat := bday]
  data[is.na(bday_hat), bday_hat := 15]
  data[, dday_hat := dday]
  data[is.na(dday_hat), dday_hat := 15]

  ## (1) birthday before (or equal to) death day --> dyear - byear
  ## NOTE: we default to this in the case where day or month are equal.
  ## * dmonth > bmonth
  ## * dmonth == bmonth & dday >= bday
  data[dmonth_hat > bmonth_hat,
    age_at_death := dyear - byear]
  data[dmonth_hat == bmonth_hat & dday_hat >= bday_hat,
    age_at_death := dyear - byear]
  ## (2) birthday after death day --> dyear - byear - 1
  ## * dmonth < bmonth
  ## * dmonth == bmonth & dday < bday
  data[dmonth_hat < bmonth_hat,
    age_at_death := dyear - byear - 1]
  data[dmonth_hat == bmonth_hat & dday_hat < bday_hat,
    age_at_death := dyear - byear -1]

  data <- data %>%
    select(dmonth_hat, bmonth_hat, dday_hat, bday_hat)

  return(data)
}

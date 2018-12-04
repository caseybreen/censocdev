#' Get first word from a string
#'
#' @param numdeath_path path to the NUMDEATH files
#' @return NUMDEATH data.frame
#' @keywords internal
#' @import data.table
#' @export


load_numdeath <- function(numdeath_path = "/data/josh/CenSoc/NUMDEATH/") {

  files <- list.files(path = numdeath_path, pattern = ".csv$")

  all_cols_to_keep <- c("ssn", "nh_name_first", "nh_name_last", "sex", "dob", "dod", "zip_residence")

  numdeath <- rbindlist(lapply(paste0(numdeath_path, files), fread, select=all_cols_to_keep, colClasses = list(character= 'ssn', 'zip_residence', 'dob', 'dod')))

  cat("Cleaning variables. \n")

  ## A. clean the socsec data
  ## shorten names by removing blank space at end
  numdeath[,"lname" := nh_name_last]
  numdeath[,"fname" := nh_name_first]
  numdeath[,lname := gsub(pattern = "\\s*$",
                          replacement = "", x = lname)]
  numdeath[,fname := gsub(pattern = "\\s*$",
                          replacement = "", x = fname)]

  ##In 162 cases, dob is 7 characters long instead of 8. In all 162 cases, the leading 0 has been ommitted.
  ## For these 162 cases, we will add a leading o.

  numdeath[, dob := ifelse(nchar(dob) == 8, dob, paste0("0", dob))]

  ## now get birth and death year
  numdeath[,"byear" := as.numeric(substr(dob, 5, 8))]
  numdeath[,"dyear" := as.numeric(substr(dod, 5, 8))]

  ## birth and death month
  numdeath[,"bmonth" := as.numeric(substr(dob, 1, 2))]
  numdeath[,"dmonth" := as.numeric(substr(dod, 1, 2))]

  ## birth and death dat
  numdeath[,"bday" := as.numeric(substr(dob, 3, 4))]
  numdeath[,"dday" := as.numeric(substr(dod, 3, 4))]

  ## now get census_age
  numdeath[,"census_age" := ifelse(bmonth < 4,
                                   1940 - byear,
                                   1939 - byear)]
  numdeath[, c("dob","dod", "nh_name_last", "nh_name_first" ):=NULL]


  return(numdeath)
}


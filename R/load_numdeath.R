#' Load numdeath file 2.0
#'
#' @param numdeath_path path to the NUMDEATH files
#' @return NUMDEATH data.frame
#' @keywords internal
#' @import data.table
#' @export


load_numdeath <- function(numdeath.file.path = "/censoc/data/numident/1_numident_files_with_original_varnames/numident_death_records_complete.csv") {

  all_cols_to_keep <- c("NUMI_SSN", "NUMI_NH_NAME_FIRST_DTH_1", "NUMI_NH_NAME_MIDDLE_DTH_1", "NUMI_NH_NAME_LAST_DTH_1", "NUMI_SEX_DTH_1", "NUMI_DOB_DTH_1", "NUMI_DOD_DTH_1", "NUMI_ZIP_RESIDENCE_1")

  numdeath <- fread(file = numdeath.file.path, select = all_cols_to_keep, colClasses = list(character= 'NUMI_SSN', 'NUMI_ZIP_RESIDENCE_1', 'NUMI_DOB_DTH_1', 'NUMI_DOD_DTH_1'))

  cat("Cleaning variables. \n")

  ## A. clean the socsec data
  ## shorten names by removing blank space at end
  numdeath[,"lname" := NUMI_NH_NAME_LAST_DTH_1]
  numdeath[,"fname" := NUMI_NH_NAME_FIRST_DTH_1]
  numdeath[,"mname" := NUMI_NH_NAME_MIDDLE_DTH_1]
  numdeath[,lname := gsub(pattern = "\\s*$",
                          replacement = "", x = lname)]
  numdeath[,fname := gsub(pattern = "\\s*$",
                          replacement = "", x = fname)]
  numdeath[,mname := gsub(pattern = "\\s*$",
                          replacement = "", x = mname)]

  ##In 162 cases, NUMI_DOB is 7 characters long instead of 8. In all 162 cases, the leading 0 has been ommitted.
  ## For these 162 cases, we will add a leading o.

  numdeath[, NUMI_DOB_DTH_1 := ifelse(nchar(NUMI_DOB_DTH_1) == 8, NUMI_DOB_DTH_1, paste0("0", NUMI_DOB_DTH_1))]

  ## now get birth and death year
  numdeath[,"byear" := as.numeric(substr(NUMI_DOB_DTH_1, 5, 8))]
  numdeath[,"dyear" := as.numeric(substr(NUMI_DOD_DTH_1, 5, 8))]

  ## birth and death month
  numdeath[,"bmonth" := as.numeric(substr(NUMI_DOB_DTH_1, 1, 2))]
  numdeath[,"dmonth" := as.numeric(substr(NUMI_DOD_DTH_1, 1, 2))]

  ## birth and death day
  numdeath[,"bday" := as.numeric(substr(NUMI_DOB_DTH_1, 3, 4))]
  numdeath[,"dday" := as.numeric(substr(NUMI_DOD_DTH_1, 3, 4))]

  ## rename SSN variables
  numdeath[, "ssn" := NUMI_SSN]

  ## rename some of the variable names
  numdeath[,sex := NUMI_SEX_DTH_1]
  numdeath[,zip_residence := NUMI_ZIP_RESIDENCE_1]

  recoded_0 <-  nrow(numdeath[NUMI_SEX_DTH_1==0,])
  numdeath[ , NUMI_SEX_DTH_1:= (ifelse(NUMI_SEX_DTH_1==0, NA, NUMI_SEX_DTH_1)) ]
  cat(recoded_0, "sex values recoded from 0 to NA. \n")

  recoded_0 <-  nrow(numdeath[dday==0,])
  numdeath[ , dday:= (ifelse(dday==0, NA, dday)) ]
  cat(recoded_0, "dday values recoded from 0 to NA. \n")

  recoded_0 <-  nrow(numdeath[bday==0,])
  numdeath[ , bday:= (ifelse(bday==0, NA, bday)) ]
  cat(recoded_0, "bday values recoded from 0 to NA. \n")

  numdeath[, c("NUMI_DOB_DTH_1","NUMI_DOD_DTH_1", "NUMI_NH_NAME_FIRST_DTH_1",
               "NUMI_NH_NAME_LAST_DTH_1", "NUMI_SSN", "NUMI_SEX_DTH_1", "NUMI_ZIP_RESIDENCE_1",
               "NUMI_NH_NAME_MIDDLE_DTH_1"):=NULL]

  numdeath[numdeath==''|numdeath==' ']<-NA

  return(numdeath)
}

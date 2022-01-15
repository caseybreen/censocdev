#' Load Application Files
#'
#' @param numapp_path path to the NUMAPP files
#' @return NUMAPP data.frame
#' @keywords internal
#' @import data.table
#' @export


clean_apps <- function(numapp.file.path = "/censoc/data/numident/1_numident_files_with_original_varnames/applications_original.csv") {

  columns.to.keep <- c("NUMI_SSN", "NUMI_CITIZENSHIP_CODE", "NUMI_CYCLE_DATE", "NUMI_ENTRY_CODE", "NUMI_DOB", "NUMI_SEX",
                       "NUMI_RACE", "NUMI_POB_STATE_COUNTRY", "NUMI_POB_FOREIGN_IND", "NUMI_NH_NAME_FIRST",
                       "NUMI_NH_NAME_MIDDLE", "NUMI_NH_NAME_LAST", "NUMI_MTH_NAME_FIRST", "NUMI_MTH_NAME_MIDDLE",
                       "NUMI_MTH_NAME_LAST", "NUMI_FTH_NAME_FIRST", "NUMI_FTH_NAME_MIDDLE", "NUMI_FTH_NAME_LAST", "NUMI_POB_CITY", "NUMI_POB_CITY_OFLO")

  numapp <-  fread(file = numapp.file.path, select = columns.to.keep)

  ## rename columns
  variable.names <- c("ssn" = "NUMI_SSN",
             "citizenship_code" = "NUMI_CITIZENSHIP_CODE",
             "cycle_date" = "NUMI_CYCLE_DATE",
             "entry_code" = "NUMI_ENTRY_CODE",
             "dob" = "NUMI_DOB",
             "sex" = "NUMI_SEX",
             "race" = "NUMI_RACE",
             "pob_state_country" = "NUMI_POB_STATE_COUNTRY",
             "pob_foreign_ind" = "NUMI_POB_FOREIGN_IND",
             "fname" = "NUMI_NH_NAME_FIRST",
             "mname" = "NUMI_NH_NAME_MIDDLE",
             "lname" = "NUMI_NH_NAME_LAST",
             "mother_fname" ="NUMI_MTH_NAME_FIRST",
             "mother_mname" = "NUMI_MTH_NAME_MIDDLE",
             "mother_lname" = "NUMI_MTH_NAME_LAST",
             "father_fname" = "NUMI_FTH_NAME_FIRST",
             "father_mname" = "NUMI_FTH_NAME_MIDDLE",
             "father_lname" = "NUMI_FTH_NAME_LAST",
             "pob_city" = "NUMI_POB_CITY",
             "pob_city_oflo" = "NUMI_POB_CITY_OFLO")

  ## rename columns
  numapp <- select(numapp, variable.names)

  ## Finished reading in columns

  cat("Finished appending numapp files", "\n")

  ## There are 398 cases where dob is only 7 characters because it is missing a leading 0.
  ## Here, we'll add a leading 0 to all dob values that are 7 characters in length.

  numapp[, dob := ifelse(nchar(dob) == 8, dob, paste0("0", dob))]

  ## Create year_cycle, month_cycle, and year_birth variables

  numapp[,"year_cycle" := as.numeric(substr(cycle_date, 1, 4))]
  numapp[,"month_cycle" := as.numeric(substr(cycle_date, 5, 6))]

  cat("Finished creating year_cycle and month_year variables \n")

  numapp <- numapp[!(grepl("ZZZZZZZZZ", numapp$ssn))]
  numapp[numapp==''|numapp==' '] <- NA

  cat("Finished removing all values with an ssn of ZZZZZZZZ (confidential) and recoding blanks to NA")

  return(numapp)

}

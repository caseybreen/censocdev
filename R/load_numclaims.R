#' Load claims file
#'
#' @param numapp_path path to the NUMAPP files
#' @return NUMAPP data.frame
#' @keywords internal
#' @import data.table
#' @export


load_numclaims <- function(numclaim.file.path = "/censoc/data/numident/numident_files_with_original_varnames/numident_claims_complete.csv") {

  columns.to.keep <- c("NUMI_SSN", "NUMI_CYCLE_DATE", "NUMI_ENTRY_CODE", "NUMI_SEX", "NUMI_DOB",
                       "NUMI_POB_STATE_COUNTRY", "NUMI_POB_FOREIGN_IND", "NUMI_NH_NAME_FIRST",
                       "NUMI_NH_NAME_MIDDLE", "NUMI_NH_NAME_LAST", "NUMI_MTH_NAME_FIRST", "NUMI_MTH_NAME_MIDDLE",
                       "NUMI_MTH_NAME_LAST", "NUMI_FTH_NAME_FIRST", "NUMI_FTH_NAME_MIDDLE", "NUMI_FTH_NAME_LAST")

  numclaim <-  fread(file = numclaim.file.path, select = columns.to.keep)

  ## rename columns
  variable.names <- c("ssn" = "NUMI_SSN",
                      "cycle_date" = "NUMI_CYCLE_DATE",
                      "entry_code" = "NUMI_ENTRY_CODE",
                      "sex" = "NUMI_SEX",
                      "dob" = "NUMI_DOB",
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
                      "father_lname" = "NUMI_FTH_NAME_LAST")

  ## rename columns
  numclaim <- select(numclaim, variable.names)

  ## Finished reading in columns

  ## There are 398 cases where dob is only 7 characters because it is missing a leading 0.
  ## Here, we'll add a leading 0 to all dob values that are 7 characters in length.

  numclaim[, dob := ifelse(nchar(dob) == 8, dob, paste0("0", dob))]

  ## Create year_cycle, month_cycle, and year_birth variables

  numclaim[,"year_cycle" := as.numeric(substr(cycle_date, 1, 4))]
  numclaim[,"month_cycle" := as.numeric(substr(cycle_date, 5, 6))]

  cat("Finished creating year_cycle and month_year variables \n")

  numclaim <- numclaim[!(grepl("ZZZZZZZZZ", numclaim$ssn))]
  numclaim[numclaim == ''| numclaim == ' ' | numclaim == "UNKNOWN"] <- NA

  cat("Finished removing all values with an ssn of ZZZZZZZZ (confidential) and recoding blanks to NA")

  return(numclaim)

}

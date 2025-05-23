#' Load numdeath file 2.0
#'
#' @param numdeath_path path to the NUMDEATH files
#' @return NUMDEATH data.frame
#' @keywords internal
#' @import data.table
#' @export

clean_deaths <- function(numdeath.file.path = "/global/scratch/p2p3/pl1_demography/censoc_internal/data/numident/1_numident_files_with_original_varnames/deaths_original.csv",
                         ssn_state_crosswalk_path = "/global/scratch/p2p3/pl1_demography/censoc_internal/data/crosswalks/ssn_to_state_crosswalk.csv") {

  all_cols_to_keep <- c("NUMI_SSN", "NUMI_NH_NAME_FIRST_DTH_1", "NUMI_NH_NAME_MIDDLE_DTH_1", "NUMI_NH_NAME_LAST_DTH_1", "NUMI_SEX_DTH_1", "NUMI_DOB_DTH_1", "NUMI_DOD_DTH_1", "NUMI_ZIP_RESIDENCE_1")

  numdeath <- fread(file = numdeath.file.path, na.strings="", select = all_cols_to_keep, colClasses = list(character= 'NUMI_SSN', 'NUMI_ZIP_RESIDENCE_1', 'NUMI_DOB_DTH_1', 'NUMI_DOD_DTH_1'))

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

  recoded_0 <-  nrow(numdeath[NUMI_SEX_DTH_1==0,])
  numdeath[ , NUMI_SEX_DTH_1:= (ifelse(NUMI_SEX_DTH_1==0, NA, NUMI_SEX_DTH_1)) ]
  cat(recoded_0, "sex values recoded from 0 to NA. \n")

  ## rename some of the variable names
  numdeath[,sex := NUMI_SEX_DTH_1]
  numdeath[,zip_residence := NUMI_ZIP_RESIDENCE_1]

  recoded_0 <-  nrow(numdeath[dday==0,])
  numdeath[ , dday:= (ifelse(dday==0, NA, dday)) ]
  cat(recoded_0, "dday values recoded from 0 to NA. \n")

  recoded_0 <-  nrow(numdeath[bday==0,])
  numdeath[ , bday:= (ifelse(bday==0, NA, bday)) ]
  cat(recoded_0, "bday values recoded from 0 to NA. \n")

  numdeath[, c("NUMI_DOB_DTH_1","NUMI_DOD_DTH_1", "NUMI_NH_NAME_FIRST_DTH_1",
               "NUMI_NH_NAME_LAST_DTH_1", "NUMI_SSN", "NUMI_SEX_DTH_1", "NUMI_ZIP_RESIDENCE_1",
               "NUMI_NH_NAME_MIDDLE_DTH_1"):=NULL]

  ## Geography Social Security Number

  ssn_state_crosswalk <- fread(ssn_state_crosswalk_path)

  numdeath[,"area_number" := as.numeric(substr(ssn, 1, 3))]
  numdeath[,"group_number" := as.numeric(substr(ssn, 4, 5))]

  ## merge and drop unnecessary columns
  ## manually add BPL requiring area number and group number
  numdeath <- numdeath %>%
    left_join(ssn_state_crosswalk, by = "area_number") %>%
    mutate(socstate = case_when(
      area_number == 580 & group_number == 20 ~ 11000,
      area_number == 586 & group_number %in% 20:28 ~ 10000,
      area_number == 586 & group_number %in% 1:18 ~ 10500,
      area_number == 580 & group_number %in% 1:18 ~ 11500,
      area_number == 232 & group_number %in% 30 ~ 3700,
      area_number == 232 & group_number != 30 ~ 5400,
      TRUE ~ as.numeric(socstate)
    )) %>%
    select(-area_number, -label, -group_number)

  numdeath[numdeath == '' | numdeath == ' '] <- NA

  return(numdeath)
}


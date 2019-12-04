#' Load application Files
#'
#' @param numapp_path path to the NUMAPP files
#' @return NUMAPP data.frame
#' @keywords internal
#' @import data.table
#' @export


load_numapp <- function(numapplic_path = "/data/josh/CenSoc/NUMIAPPLIC/Records/") {

  files <- list.files(path = numapplic_path, pattern = ".csv$")

  ## Append all 20 numapp files into one large file and only keep certain variables

  all_cols_to_keep <- c("ssn", "citizenship_code", "entry_code", "cycle_date", "dob", "zip_residence", "sex", "race",
                        "pob_state_country", "pob_foreign_ind", "dob_change_ind", "prior_dob",
                        "nh_name_first", "nh_name_middle", "nh_name_last", "nh_name_suffix",
                        "mth_name_first", "mth_name_middle", "mth_name_last", "mth_name_suffix",
                        "fth_name_first", "fth_name_middle", "fth_name_last", "fth_name_suffix",
                        "oth_name_middle", "oth_name_last", "oth_name_suffix", "sig_name_first",
                        "sig_name_middle", "sig_name_last", "sig_name_suffix", "sig_name_code")

  numapplic_append = rbindlist(lapply(paste0(numapplic_path, files), fread, select=all_cols_to_keep, colClasses = list(character= 'ssn')))

  ## Finished reading in columns

  cat("Finished appending numapp files", "\n")

  ## There are 398 cases where dob is only 7 characters because it is missing a leading 0.
  ## Here, we'll add a leading 0 to all dob values that are 7 characters in length.

  numapplic_append[, dob := ifelse(nchar(dob) == 8, dob, paste0("0", dob))]

  ## Create year_cycle, month_cycle, and year_birth variables

  numapplic_append[,"year_cycle" := as.numeric(substr(cycle_date, 1, 4))]
  numapplic_append[,"month_cycle" := as.numeric(substr(cycle_date, 5, 6))]

  cat("Finished creating year_cycle and month_year variables \n")

  numapplic_append <- numapplic_append[!(grepl("ZZZZZZZZZ", numapplic_append$ssn))]
  numapplic_append[numapplic_append==''|numapplic_append==' ']<-NA

  cat("Finished removing all values with an ssn of ZZZZZZZZ (confidential) and recoding blanks to NA")


  return(numapplic_append)

}

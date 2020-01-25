#' Load a census file
#'
#' @param census_file string indicating location of file.
#' @param cols_to_keep vector indicating colname names of variables to keep. Default is just what is needed for match: first name, last name, age, sex, serial number, person number.
#' @return a census dataframe with match key and unique IPUMS identifier
#' @import data.table
#' @keywords internal
#' @export

load_census_numident_match <- function(census_file,
                        cols_to_keep = NULL){

  # Select Columns

  all_cols_to_keep <- c("STATEFIP", "AGE", "SEX", "NAMELAST", "NAMEFRST", "BPL", "RACE", "MARST", "HISTID")
  if(!is.null(cols_to_keep)){
    all_cols_to_keep <- c(all_cols_to_keep, cols_to_keep)
  }

  # read in census file, only keeping desired columns
  census <- fread(census_file, select = all_cols_to_keep)
  census <- as.data.table(census)

  # clean first name variables
  census[,"fname" := enc2native(NAMEFRST)]
  census[,"fname" := toupper(fname)]
  census[,"fname" := get_first_word(fname)]

  ## clean last name variables
  census[,"lname" := enc2native(NAMELAST)]
  census[,"lname" := toupper(lname)]
  census[,"lname" := get_first_word(lname)]

  ## set age to numeric
  census[,"census_age" := as.numeric(AGE)]

  ## remove those with no name info
  ## remove blanks or anything that has question marks
  census <- census[!(grepl("\\?", census$fname)|grepl("\\?", census$lname)|census$lname==""),]

  ## create key
  census[,"linking_key" := paste(lname, fname, census_age, BPL, sep = "_")]
  census[,"linking_key" := clean_key(linking_key),]

  ## remove the links
  census <- census %>%
    select(-census_age, -fname, -lname)

  return(census)

}

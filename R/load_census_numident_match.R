#' Load a census file
#'
#' @param census_file string indicating location of file.
#' @param cols_to_keep vector indicating colname names of variables to keep. Default is just what is needed for match: first name, last name, age, sex, serial number, person number.
#' @param males_only whether to just keep males. Default is \code{TRUE}.
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

  # clean variables
  census[,"fname" := enc2native(NAMEFRST)]
  census[,"fname" := toupper(fname)]
  census[,"fname" := get_first_word(fname)]
  census[,"lname" := enc2native(NAMELAST)]
  census[,"lname" := toupper(lname)]
  census[,"census_age" := as.numeric(AGE)]

  # remove those with no name info
  # remove blanks or anything that has question marks
  census <- census[!(grepl("\\?", census$fname)|grepl("\\?", census$lname)|census$lname==""),]

  # create key
  census[,"linking_key" := paste(lname_census, fname_census, census_age, BPL, sep = "_")]
  census[,"linking_key" := clean_key(linking_key),]

  return(census)

}

#' This script appends geography variables onto the numdeath files
#'
#' @param numdeath_path path to the NUMDEATH files
#' @return NUMDEATH data.frame
#' @keywords internal
#' @import data.table
#' @export


load_numdeath_geo <- function(numdeath, ssn_state_codes = "/home/ipums/casey-ipums/censoc/data/ssn_state_codes.csv", zip_code_states = "/home/ipums/casey-ipums/zip_code_states.csv") {

  numdeath <- numdeath

  numdeath[,"ssn_3" := as.numeric(substr(ssn, 1, 3))]

  ## read in state social security codes
  ssn_state_codes <- fread(ssn_state_codes)

  ## set merge keys
  setkey(numdeath, ssn_3)
  setkey(ssn_state_codes, ssn_3)

  ## merge on unique keys
  numdeath <- merge(numdeath, ssn_state_codes, on = key, all.x = TRUE)
  numdeath[,ssn_3:=NULL]

  cat("State from ZIP codes complete \n")

  ## create zip codes
  numdeath[,"zip_code_5" := as.character(substr(zip_residence, 1, 5))]

  ## read in zip code dataset
  zip_code <- fread(zip_code_states, colClasses = list(character= 'zip_code_5'))

  ## set merge keys
  setkey(zip_code, zip_code_5)
  setkey(numdeath, zip_code_5)


  ## merge on unique keys
  numdeath <- merge(numdeath, zip_code, on = zip_code_5, all.x = TRUE)

  cat("City and State from ZIP codes complete \n")


  return(numdeath)

}

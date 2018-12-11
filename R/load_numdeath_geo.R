#' Get first word from a string
#'
#' @param numdeath_path path to the NUMDEATH files
#' @return NUMDEATH data.frame
#' @keywords internal
#' @import data.table
#' @export


load_numdeath_geo <- function(numdeath, ssn_state_codes = "/nobackup/90days/andreamg/ssn_state_codes.csv",
                              zip_code_states = "/nobackup/90days/andreamg/zip_code_states.csv",
                              state_fip_codes = "/nobackup/90days/andreamg/state_fips_ipums.csv") {

  numdeath <- numdeath

  ## read in state social security codes
  ssn_state_codes <- fread(ssn_state_codes)

  ## Read in zip code dataset
  zip_code <- fread(zip_code_states, colClasses = list(character= 'zip_code_5'))

  ## Read in state FIP codes. The State FIP codes will be as the numeric codes to represent states
  state_fips <- fread(state_fip_codes)

  numdeath[,"ssn_3" := as.numeric(substr(ssn, 1, 3))]

  ## set merge keys
  setkey(numdeath, ssn_3)
  setkey(ssn_state_codes, ssn_3)


  ## merge on unique keys
  numdeath <- merge(numdeath, ssn_state_codes, on = key, all.x = TRUE)
  numdeath[,ssn_3:=NULL]

  cat("State from ZIP codes complete")

  ## create zip codes
  numdeath[,"zip_code_5" := as.character(substr(zip_residence, 1, 5))]


  ## set merge keys
  setkey(zip_code, zip_code_5)
  setkey(numdeath, zip_code_5)

  ## merge on unique keys
  numdeath <- merge(numdeath, zip_code, on = zip_code_5, all.x = TRUE)

  cat("City and State from ZIP codes complete\n")

  ## merge on unique keys
  numdeath <- merge(numdeath, state_fips, by.x="socstate",by.y="state_label", all.x = TRUE)
  numdeath <- numdeath[, -c("socstate", "state_code")]
  setnames(numdeath, old="ipums_state", new="socstate")

  numdeath <- merge(numdeath, state_fips, by.x="dstate",by.y="state_code", all.x = TRUE)
  numdeath <- numdeath[, -c("dstate", "state_label")]
  setnames(numdeath, old="ipums_state", new="dstate")

  cat("State from FIP codes complete\n")

  numdeath[,zip_code_5 := NULL]

  return(numdeath)

}

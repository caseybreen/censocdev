#' Build Numapp File
#'
#' @param numapp path to the NUMAPP files
#' @return NUMAPP data.frame condensed
#' @keywords internal
#' @import data.table
#' @export

condense_numapp <- function(numapp = numapp) {

  numapp <- numapp

  sex <- select_sex(numapp)

  race <- select_race(numapp)

  best_first_name <- select_best_first_name(numapp)

  best_last_name <- select_best_last_name(numapp)

  best_father_last_name <- select_best_father_last_name(numapp)

  #best_birth_place <- select_best_birth_place(numapp)

  unique_ssn <- numapp[,"ssn", with=FALSE]
  unique_ssn <- unique(unique_ssn, by = "ssn")

  numapp_condensed = Reduce(function(...) merge(..., all = TRUE), list(unique_ssn, sex, race, best_first_name, best_last_name, best_father_last_name))

  return(numapp_condensed)

}

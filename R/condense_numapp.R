#' Build Numapp File
#'
#' @param numapp path to the NUMAPP files
#' @return NUMAPP data.frame condensed
#' @keywords internal
#' @import data.table
#' @export

condense_numapp <- function(numapp = numapp) {

  ## Assign numapp
  numapp <- numapp

  ## Select sex
  sex <- select_sex(numapp)
  cat("finished selecting Sex \n")

  ##Select Race

  race <- select_race(numapp)
  cat("finished selecting Race \n")

  ##Select Best First Name
  best_first_name <- select_best_first_name(numapp)
  cat("finished selecting first name \n")

  ##Select Best Last Name
  best_last_name <- select_best_last_name(numapp)
  cat("finished selecting last name \n")

  ##Select Best Father Last Name
  best_father_last_name <- select_best_father_last_name(numapp)
  cat("finished selecting father last name \n")

  #bpl <- select_best_birth_place(numapp)

  ##Select Best Father Last Name
  dob <- select_dob(numapp)
  cat("finished selecting father last name \n")

  unique_ssn <- numapp[,"ssn", with=FALSE]
  unique_ssn <- unique(unique_ssn, by = "ssn")

  numapp_condensed = Reduce(function(...) merge(..., all = TRUE), list(unique_ssn, sex, race, best_first_name, best_last_name, best_father_last_name, dob, bpl))

  return(numapp_condensed)

}

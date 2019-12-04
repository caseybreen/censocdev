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
  best_first_name <- select_longest_name(data = numapp, name = "fname")
  cat("finished selecting first name \n")

  ##Select Middle Name
  best_middle_name <- select_longest_name(data = numapp, name = "mname")
  cat("finished selecting best middle name \n")

  ##Select Best Last Name
  best_last_name <- select_longest_name(data = numapp, name = "lname")
  cat("finished selecting last name \n")

  ##Select Best Father First Name
  best_father_first_name <- select_longest_name(data = numapp, name = "father_fname")
  cat("finished selecting father first name \n")

  ##Select Best Father Middle Name
  best_father_middle_name <- select_longest_name(data = numapp, name = "father_mname")
  cat("finished selecting father middle name \n")

  ##Select Best Father Last Name
  best_father_last_name <- select_longest_name(data = numapp, name = "father_lname")
  cat("finished selecting father last name \n")

  ##Select Best Mother First Name
  best_mother_first_name <- select_longest_name(data = numapp, name = "mother_fname")
  cat("finished selecting mother first name \n")

  ##Select Best Mother Middle Name
  best_mother_middle_name <- select_longest_name(data = numapp, name = "mother_mname")
  cat("finished selecting mother middle name \n")

  ##Select Best Mother Last Name
  best_mother_last_name <- select_longest_name(data = numapp, name = "mother_lname")
  cat("finished selecting mother last name \n")

  ##Select Best Birth Place
  bpl <- select_birthplace(numapp)
  cat("finished selecting best birthplace last name \n")

  ##Select Best Date of Birth
  dob <- select_dob(numapp)
  cat("finished selecting best dob last name \n")

  ## Select Earliest Application Date
  earliest_app <- select_date_of_first_app(numapp)
  cat("finished selecting earliest application \n")

  ## Select Number of Applications
  number_of_apps <- select_number_of_apps(numapp)
  cat("finished selecting total number of applications per person \n")

  ## Select unique SSN Numbers
  unique_ssn <- numapp[,"ssn", with=FALSE]
  unique_ssn <- unique(unique_ssn, by = "ssn")

  ## Combine "Best" fname, lname, etc. into one data.frame
  numapp_condensed <-  Reduce(function(...) merge(..., all = TRUE), list(unique_ssn, sex, race,
                                                                         best_first_name, best_middle_name, best_last_name,
                                                                         best_father_first_name, best_father_middle_name, best_father_last_name,
                                                                         best_mother_first_name, best_mother_middle_name, best_mother_last_name,
                                                                         dob, bpl, earliest_app, number_of_apps))


  return(numapp_condensed)

}

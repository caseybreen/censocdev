#' Build Numapp File
#'
#' @param numapp path to the NUMAPP files
#' @return NUMAPP data.frame condensed
#' @keywords internal
#' @import data.table
#' @export

condense_claims <- function(claims = claims) {

  ## Select sex
  sex <- select_sex(claims)
  cat("finished selecting Sex \n")

  ##Select Best First Name
  best_first_name <- select_longest_name(data = claims, name = "fname")
  cat("finished selecting first name \n")

  ##Select Middle Name
  best_middle_name <- select_longest_name(data = claims, name = "mname")
  cat("finished selecting best middle name \n")

  ##Select Best Last Name
  best_last_name <- select_longest_name(data = claims, name = "lname")
  cat("finished selecting last name \n")

  ##Select Best Father First Name
  best_father_first_name <- select_longest_name(claims, name = "father_fname")
  cat("finished selecting father first name \n")

  ##Select Best Father Middle Name
  best_father_middle_name <- select_longest_name(claims, name = "father_mname")
  cat("finished selecting father middle name \n")

  ##Select Best Father Last Name
  best_father_last_name <- select_longest_name(claims, name = "father_lname")
  cat("finished selecting father last name \n")

  ##Select Best Mother First Name
  best_mother_first_name <- select_longest_name(claims, name = "mother_fname")
  cat("finished selecting mother first name \n")

  ##Select Best Mother Middle Name
  best_mother_middle_name <- select_longest_name(claims, name = "mother_mname")
  cat("finished selecting mother middle name \n")

  ##Select Best Mother Last Name
  best_mother_last_name <- select_longest_name(claims, name = "mother_lname")
  cat("finished selecting mother last name \n")

  ##Select Best Birth Place
  bpl <- select_birthplace(claims)
  cat("finished selecting best birthplace last name \n")

  ##Select Best Date of Birth
  dob <- select_dob(claims)
  cat("finished selecting best dob last name \n")

  ##Select Best Date of Birth
  number_claims <- select_number_claims(claims)
  cat("finished selecting number of claims \n")

  ## Select unique SSN Numbers
  unique_ssn <- claims[,"ssn", with=FALSE]
  unique_ssn <- unique(unique_ssn, by = "ssn")

  ## Combine "Best" fname, lname, etc. into one data.frame
  claims_condensed <-  Reduce(function(...) merge(..., all = TRUE), list(unique_ssn, sex,
                                                               best_first_name, best_middle_name, best_last_name,
                                                               best_father_first_name, best_father_middle_name, best_father_last_name,
                                                               best_mother_first_name, best_mother_middle_name, best_mother_last_name,
                                                               dob, bpl, number_claims))

  ## Recode any blanks to NA
  claims_condensed[claims_condensed == ''| claims_condensed == ' '] <- NA

  return(claims_condensed)

}

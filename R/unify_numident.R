#' Unify Numident Files
#'
#' @param numapp path to the NUMAPP files
#' @return Unify Numident Files
#' @keywords internal
#' @import data.table
#' @export
#'
unify_numident <- function(claim_condensed, deaths, application_condensed) {

  ## Read in death file
  deaths <- deaths %>%
    select(ssn, sex, zip_residence, lname, mname, fname, byear, dyear,
           bmonth, dmonth, bday, dday) %>%
    mutate(ssn = as.numeric(ssn))

  ## Read in application file (condensed)
  application_condensed <- application_condensed %>%
    select(ssn, bpl, earliest_cycle_year, earliest_cycle_month,
           mother_fname, mother_mname, mother_lname,
           father_fname, father_mname, father_lname, number_of_apps,
           race, sex, race_change = race_multiple_flag) %>%
    mutate(ssn = as.numeric(ssn))

  ## Read in claims file (condensed)

  claim_condensed <- claim_condensed %>%
    select(sex, ssn, bpl, mother_fname, mother_mname, mother_lname, father_fname, father_mname, father_lname) %>%
    mutate(ssn = as.numeric(ssn))

  numident <- deaths %>%
    left_join(application_condensed, by = "ssn")

  ## Replace any values of NA for sex with respective values from application files.
  ## Coalese function documentation: https://dplyr.tidyverse.org/reference/coalesce.html

  numident <- numident %>%
    mutate(sex = coalesce(sex.x, sex.y)) %>%
    select(-c(sex.x, sex.y))

  ## Replace any values of NA for sex, bpl, parents' names with respective values from claims files.

  numident <- numident %>%
    left_join(claim_condensed, by = "ssn") %>%
    mutate(sex = coalesce(sex.x, sex.y)) %>%
    mutate(bpl = coalesce(bpl.x, bpl.y)) %>%
    mutate(father_fname = coalesce(father_fname.x, father_fname.y)) %>%
    mutate(father_mname = coalesce(father_mname.x, father_mname.y)) %>%
    mutate(father_lname = coalesce(father_lname.x, father_lname.y)) %>%
    mutate(mother_fname = coalesce(mother_fname.x, mother_fname.y)) %>%
    mutate(mother_mname = coalesce(mother_mname.x, mother_mname.y)) %>%
    mutate(mother_lname = coalesce(mother_lname.x, mother_lname.y)) %>%
    select(-c(sex.x, sex.y,
              bpl.x, bpl.y,
              father_fname.x, father_fname.y,
              father_mname.x, father_mname.y,
              father_lname.x, father_lname.y,
              mother_fname.x, mother_fname.y,
              mother_mname.x, mother_mname.y,
              mother_lname.x, mother_lname.y))

  ## Calculate age at first social security application

  numident <- numident %>%
    mutate(age_first_application = case_when(
      bmonth >= earliest_cycle_month ~ earliest_cycle_year - byear,
      TRUE ~ earliest_cycle_year - byear - 1
    ))

  ## Calculate age at Death

  numident <- numident %>%
    calculate_age_at_death()

  return(numident)
}


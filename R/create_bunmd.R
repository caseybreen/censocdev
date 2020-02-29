#' Unify Numident Files
#'
#' @param numapp path to the NUMAPP files
#' @return Unify Numident Files
#' @keywords internal
#' @import data.table
#' @export
#'
create_bunmd <- function(claims_condensed, deaths, apps_condensed) {

  ## select vars from death file
  deaths <- deaths %>%
    select(ssn, sex, zip_residence, lname, mname, fname, byear, dyear, socstate,
           bmonth, dmonth, bday, dday) %>%
    mutate(ssn = as.numeric(ssn))

  ## select vars from application file
  application <- apps_condensed %>%
    select(ssn, bpl, earliest_cycle_year, earliest_cycle_month,
           mother_fname, mother_mname, mother_lname,
           father_fname, father_mname, father_lname, number_apps = number_of_apps,
           race_first, race_last, race_last_cyear = race_last_year, race_first_cyear = race_first_year, race_last_cmonth = race_last_month, race_first_cmonth = race_first_month, sex, race_change = race_multiple_flag) %>%
    mutate(ssn = as.numeric(ssn))

  ## select vars from claims files (condensed)
  claim <- claims_condensed %>%
    select(sex, ssn, bpl, mother_fname, mother_mname, mother_lname, father_fname, father_mname, father_lname, number_claims = number_of_claims) %>%
    mutate(ssn = as.numeric(ssn))

  bunmd <- deaths %>%
    left_join(application, by = "ssn")

  ## Replace any values of NA for sex with respective values from application files.
  ## Coalese function documentation: https://dplyr.tidyverse.org/reference/coalesce.html

  bunmd <- bunmd %>%
    mutate(sex = coalesce(sex.x, sex.y)) %>%
    select(-c(sex.x, sex.y))

  ## Replace any values of NA for sex, bpl, parents' names with respective values from claims files.
  bunmd <- bunmd %>%
    left_join(claim, by = "ssn") %>%
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
  setDT(bunmd)
  bunmd[,"age_first_application" := ifelse(bmonth >= earliest_cycle_month,
                                 earliest_cycle_year - byear - 1,
                                 earliest_cycle_year - byear)]

  ## Recode NA for number_claims and number_apps to 0
  bunmd[ , number_claims:= (ifelse(is.na(number_claims), 0, number_claims)) ]
  bunmd[ , number_apps:= (ifelse(is.na(number_apps), 0, number_apps)) ]

  ## Calculate age at Death
  bunmd <- bunmd %>%
    calculate_age_at_death()

  ## Recode blanks to NAs
  bunmd[bunmd==''|bunmd ==' '] <- NA

  ## drop earliest cycle month/year variables
  bunmd <- bunmd %>%
    select(-earliest_cycle_year, -earliest_cycle_month)

  return(bunmd)
}

#' Unify Numident Files
#'
#' @param numapp path to the NUMAPP files
#' @return Unify Numident Files
#' @keywords internal
#' @import data.table
#' @export
#'
unify_numident <- function(application, deaths, claim) {

  deaths <- death %>%
    select(ssn, sex, zip_residence, lname, mname, fname, byear = byear_death_file, dyear,
           bmonth = bmonth_death_file, dmonth, bday = bday_death_file, dday, socstate)

  application <- application %>%
    select(ssn, bpl, earliest_cycle_year, father_lname, number_of_apps, race, sex, sex_change = sex_multiple_flag, race_change = race_multiple_flag)

  claims <- claims %>%
    select(ssn, bpl, father_lname)

  numident <- deaths %>%
    left_join(application, by = "ssn")

  numident <- numident %>%
    mutate(sex = coalesce(sex.x, sex.y))

  numident <- numident %>%
    left_join(claims) %>%
    mutate(sex = coalesce(sex.x, sex.y)) %>%
    mutate(bpl = coalesce(bpl.x, bpl.y)) %>%
    mutate(father_lname = coalesce(father_lname.x, father_lname.y) %>%
    select(-c(sex.x, sex.y, bpl.x, bpl.y, father_lname.x, father_lname.y))

  return(numident)
}


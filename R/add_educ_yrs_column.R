#' Add column with recoded education to a CenSoc data table
#'
#' Recode education variable.
#'
#'@param df a CenSoc data frame. Must have `EDUC` column.
#'@return a CenSoc data frame with `educ_yrs` column.
#'@export

add_educ_yrs_column <- function(censoc.df){

  censoc.df <- censoc.df %>%
    mutate(educyrs = case_when(
      EDUC == 2 ~ 0,
      EDUC == 22 ~ 6,
      EDUC == 26 ~ 9,
      EDUC == 40 ~ 11,
      EDUC == 50 ~ 12,
      EDUC == 30 ~ 10,
      EDUC == 25 ~ 8,
      EDUC == 14 ~ 2,
      EDUC == 15 ~ 3,
      EDUC == 16 ~ 4,
      EDUC == 100 ~ 17,
      EDUC == 110 ~ 18,
      EDUC == 17 ~ 5,
      EDUC == 23 ~ 7,
      EDUC == 60 ~ 13,
      EDUC == 70 ~ 14,
      EDUC == 90 ~ 16,
      EDUC == 80 ~ 15,
      EDUC == 12 ~ 1,
      TRUE ~ NA_real_
    ))

  return(censoc.df)
}


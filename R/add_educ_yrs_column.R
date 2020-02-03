#' Add column with recoded education to a CenSoc data table
#'
#' Recode education variable.
#'
#'@param df a CenSoc data frame. Must have `EDUC` column.
#'@return a CenSoc data frame with `educ_yrs` column.
#'@export

add_educ_yrs_column <- function(censoc.df){

  censoc.df <- censoc.df %>%
    mutate(educ_yrs = case_when(
      EDUC == 2 ~ 0,
      EDUC == 22 ~ 5,
      EDUC == 26 ~ 8,
      EDUC == 40 ~ 10,
      EDUC == 50 ~ 11,
      EDUC == 30 ~ 9,
      EDUC == 25 ~ 7,
      EDUC == 999 ~ 999,
      EDUC == 14 ~ 1,
      EDUC == 15 ~ 2,
      EDUC == 16 ~ 3,
      EDUC == 100 ~ 16,
      EDUC == 110 ~ 17,
      EDUC == 17 ~ 4,
      EDUC == 23 ~ 6,
      EDUC == 60 ~ 12,
      EDUC == 70 ~ 13,
      EDUC == 90 ~ 15,
      EDUC == 80 ~ 14,
      EDUC == 12 ~ 0,
      TRUE ~ NA_real_
    ))

  return(censoc.df)
}


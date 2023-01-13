#' recode education
#' return data.frame with education recoded
#'
#' @param data data.frame with birth and death info
#' @return data.frame
#' @keywords internal
#' @import data.table
#' @export
#'

## function for recoding education
recode_education <- function(df, educ_var) {

  educ_var <- enquo(educ_var)

  df <- df  %>%
    mutate(educ_variable = !!educ_var) %>%
    mutate(educ_yrs = case_when(
      educ_variable == 2 ~ 0,
      educ_variable == 12 ~ 0,
      educ_variable == 14 ~ 1,
      educ_variable == 15 ~ 2,
      educ_variable == 16 ~ 3,
      educ_variable == 17 ~ 4,
      educ_variable == 22 ~ 5,
      educ_variable == 23 ~ 6,
      educ_variable == 25 ~ 7,
      educ_variable == 26 ~ 8,
      educ_variable == 30 ~ 9,
      educ_variable == 40 ~ 10,
      educ_variable == 50 ~ 11,
      educ_variable == 60 ~ 12,
      educ_variable == 70 ~ 13,
      educ_variable == 80 ~ 14,
      educ_variable == 90 ~ 15,
      educ_variable == 100 ~ 16,
      educ_variable == 110 ~ 17,
      educ_variable == 111 ~ 17,
      educ_variable == 112 ~ 17,
      educ_variable == 113 ~ 17
    )) %>%
    select(-educ_variable)

  return(df)

}

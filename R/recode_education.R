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
    mutate(educ = !!educ_var) %>%
    mutate(educ_yrs = case_when(
      educ == 2 ~ 0,
      educ == 12 ~ 0,
      educ == 14 ~ 1,
      educ == 15 ~ 2,
      educ == 16 ~ 3,
      educ == 17 ~ 4,
      educ == 22 ~ 5,
      educ == 23 ~ 6,
      educ == 25 ~ 7,
      educ == 26 ~ 8,
      educ == 30 ~ 9,
      educ == 40 ~ 10,
      educ == 50 ~ 11,
      educ == 60 ~ 12,
      educ == 70 ~ 13,
      educ == 80 ~ 14,
      educ == 90 ~ 15,
      educ == 100 ~ 16,
      educ == 110 ~ 17,
      educ == 111 ~ 17,
      educ == 112 ~ 17,
      educ == 113 ~ 17
    ))

  return(df)

}

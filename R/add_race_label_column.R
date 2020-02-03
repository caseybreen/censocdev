#' Add column with recoded race to a CenSoc data table
#'
#' Recode race variable.
#'
#'@param df a CenSoc data frame. Must have `RACE` column.
#'@return a CenSoc data frame with `race_recode` column.
#'@export

add_race_label_column <- function(censoc.df){

  censoc.df <- censoc.df %>%
    mutate(race_label = case_when(
      RACE == 100 ~ "White",
      RACE == 200 ~ "Black",
      RACE == 300 ~ "American Indian or Alaska Native",
      RACE == 400 ~ "Chinese",
      RACE == 600 ~ "Other Asian or Pacific Islander",
      RACE == 210 ~ "Black",
      RACE == 500 ~ "Japanese",
      RACE == 620 ~ "Other Asian or Pacific Islander",
      RACE == 610 ~ "Other Asian or Pacific Islander",
      RACE == 634~ "Other Asian or Pacific Islander",
      RACE == 685 ~ "Other Asian or Pacific Islander",
      RACE == 371 ~ "American Indian or Alaska Native",
      RACE == 372 ~ "American Indian or Alaska Native",
      RACE == 630~ "Other Asian or Pacific Islander",
      RACE == 354~ "American Indian or Alaska Native",
      RACE == 680~ "Other Asian or Pacific Islander",
      TRUE ~ "NA"
    ))
  return(censoc.df)
}

#' Date of first application
#'
#' @param numapp path to the NUMAPP files
#' @return NUMAPP data.frame with number of applications
#' @keywords internal
#' @import data.table
#' @export

calculate_age_at_first_app <- function(numapp_condensed = numapp) {

  numapp[,"age_at_first_app" := ifelse(bmonth < earliest_cycle_month,
                                       earliest_cycle_year - byear,
                                       earliest_cycle_year - byear - 1)]

  return(numapp)
}


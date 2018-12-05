#' Merge Application and Death Files
#'
#' @param numapp path to the NUMAPP files
#' @return NUMAPP data.frame with best first name column for each unique ssn (40870456) and cycle dates
#' @keywords internal
#' @import data.table
#' @export
#'
select_best_first_name <- function(numapp = numapp, numdeath = numdeath) {

  numapp <- numapp
  numdeath <- numdeath

  merge(numdeath, numapp, by = "ssn")

  return(ss5)
}

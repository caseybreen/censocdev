#' Match household variables
#'
#' @param wcensoc a wcensoc file
#' @return Cleaned Wcensoc
#' @keywords internal
#' @import data.table
#' @export
#'
#'
add_household_variables <- function(censoc, household_census){

  ## data.table requires that we set the primary "key" of each datatable which it will use in the inner join
  setkey(household_census, SERIAL)
  setkey(censoc, SERIALP)

  ## this is the join command ridiculously simple and fast
  wcensoc <- censoc[household_census, nomatch=0]

  return(wcensoc)
}


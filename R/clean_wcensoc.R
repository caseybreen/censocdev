#' Clean WCenSoc
#'
#' @param wcensoc a wcensoc file
#' @return Cleaned Wcensoc
#' @keywords internal
#' @import data.table
#' @export
#'
#'
clean_wcensoc <- function(wcensoc, census_year){
  
  ## Remove rows where dyear is greater than byear
  wcensoc <- wcensoc[dyear >= byear]
  
  ## Remove rows where dyear is after census
  ## this should never happen 
  wcensoc <- wcensoc[dyear >= census_year]
  
  return(wcensoc)
}


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
  
  ## Keep rows where dyear is greater than byear
  wcensoc <- wcensoc[dyear >= byear]
  
  ## Keep rows where dyear is after census
  ## The reverse should never happen 
  wcensoc <- wcensoc[dyear >= census_year]
  
  return(wcensoc)
}


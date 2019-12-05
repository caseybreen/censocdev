#' Create weights
#' @param data data.frame with birth and death info
#' @return data.frame
#' @keywords internal
#' @import data.table
#' @export
#'

create_non_prob_weights <- function(numident_file) {

  hmd_deaths <-  readHMDweb(CNTRY = "USA", item = "Deaths_1x1", username ="caseybreen@berkeley.edu",
                            password = "censoc") %>% mutate(linking_key = paste(Year, Age, sep = "_" ))


  return(data)
}

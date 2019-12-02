#' Calculate total number of applications per individual
#'
#' @param numapp path to the NUMAPP files
#' @return NUMAPP data.frame with number of applications by ssn
#' @keywords internal
#' @import data.table
#' @export

select_number_of_apps <- function(numapp = numapp) {

  ## Select variables from Num Application
  numapp <- numapp[, c("ssn"), with=FALSE]

  ## Remove applications with NA value for ssn
  applications <- nrow(numapp)
  numapp <- na.omit(numapp, cols="ssn")
  removed_na <- applications - nrow(numapp)
  cat(removed_na, "removed with NA value for ssn", "\n")

  ## Remove applications with non-alphanumeric value for ssn
  applications <- nrow(numapp)
  numapp <- numapp[!(grepl("\\?", numapp$ssn))]
  removed_na <- applications - nrow(numapp)
  cat(removed_na, "removed with non-alphanumeric values for ssn", "\n")

  ## Remove applications with ZZZ values for ssn
  applications <- nrow(numapp)
  numapp <- numapp[!(grepl("ZZZ", numapp$ssn))]
  removed_na <- as.integer(applications - nrow(numapp))
  cat(removed_na, "removed with ZZZ values for ssn", "\n")

  ## Number of different social security apps per SSN
  numapp[,number_of_apps := .N, by=ssn]
  numapp <- unique(numapp)


  ## Create df with specific data.table features
  numapp_number_of_apps <- numapp[, c("ssn", "number_of_apps"), with=FALSE]

  return(numapp_number_of_apps)

}

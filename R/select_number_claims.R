#' Calculate total number of claims per individual
#'
#' @param numapp path to the claims files
#' @return NUMAPP data.frame with number of applications by ssn
#' @keywords internal
#' @import data.table
#' @export

select_number_claims <- function(data = claims) {

  ## Select variables from Num Application
  data <- data[, c("ssn"), with=FALSE]

  ## Remove applications with NA value for ssn
  applications <- nrow(data)
  data <- na.omit(data, cols="ssn")
  removed_na <- applications - nrow(data)
  cat(removed_na, "removed with NA value for ssn", "\n")

  ## Remove applications with non-alphanumeric value for ssn
  applications <- nrow(data)
  data <- data[!(grepl("\\?", data$ssn))]
  removed_na <- applications - nrow(data)
  cat(removed_na, "removed with non-alphanumeric values for ssn", "\n")

  ## Remove applications with ZZZ values for ssn
  applications <- nrow(data)
  data <- data[!(grepl("ZZZ", data$ssn))]
  removed_na <- as.integer(applications - nrow(data))
  cat(removed_na, "removed with ZZZ values for ssn", "\n")

  ## Number of different social security apps per SSN
  data[,number_of_claims := .N, by=ssn]
  data <- unique(data)


  ## Create df with specific data.table features
  data.df <- data[, c("ssn", "number_of_claims"), with=FALSE]

  return(data.df)

}

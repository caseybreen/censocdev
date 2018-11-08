#' Get first word from a string
#'
#' @param numdeath_path path to the NUMDEATH files
#' @return NUMDEATH data.frame
#' @keywords internal


create_numdeath <- function(numdeath_path = "/data/josh/CenSoc/NUMDEATH/") {

  setwd(numdeath_path)
  files <- list.files(pattern = ".csv$")

  all_cols_to_keep <- c("ssn", "nh_name_first", "nh_name_last", "sex", "dob", "dod")

  numdeath_append = rbindlist(lapply(files, fread, select=all_cols_to_keep, colClasses = list(character= 'ssn')))

  numdeath_append[,"year_death" := as.numeric(substr(dod, 5, 8))]
  numdeath_append[,"year_birth" := as.numeric(substr(dob, 5, 8))]

  # let's fix this
  nrow(numdeath_append[grepl(",+", numdeath_append$nh_name_first),])
  numdeath_append = numdeath_append %>% mutate(nh_name_first=str_replace(nh_name_first,",+", ""))

  # let's fix this
  nrow(numdeath_append[grepl(",+", numdeath_append$nh_name_last),])
  numdeath_append = numdeath_append %>% mutate(nh_name_last=str_replace(nh_name_last, ",+", ""))

  return(numdeath_append)
}

#' Load a census file
#'
#' @param socsec_files a vector of file names where the social security death files live.
#' @return a dataframe with match key, age, and date of death
#' @import data.table
#' @export

load_socsec_deaths <- function(socsec_files){

  socsec_list<- list()
  for(i in 1:length(socsec_files)){
    socsec_file <- socsec_files[i]
    cat(paste0("Reading in file ", i, ".\n"))
    tt <- read_fwf(socsec_file,
                          fwf_widths(c(1,9, 20, 4, 15, 15, 1, 8, 8),
                                     col_names = c("mode", "ssn", "lname",
                                                   "name_suffix", "fname", "mname",
                                                   "vorpcode", "dod", "dob")))
    assign(paste0("socsec",i) , as.data.table(tt))
    socsec_list[[i]] <-eval(parse(text = paste0("socsec",i)))
    rm(tt)
  }

  socsec <- rbindlist(socsec_list)
  rm(socsec_list, socsec1, socsec2, socsec3)

  cat("Cleaning variables. \n")
  ## A. clean the socsec data
  ## shorten names by removing blank space at end
  socsec[,"lname_orginal_socsec" := lname]
  socsec[,"fname_orginal_socsec" := fname]
  socsec[,lname := gsub(pattern = "\\s*$",
                        replacement = "", x = lname)]
  socsec[,fname := gsub(pattern = "\\s*$",
                        replacement = "", x = fname)]

  ## now get birth and death year
  socsec[,"byear" := as.numeric(substr(dob, 5, 8))]
  socsec[,"dyear" := as.numeric(substr(dod, 5, 8))]
  ## birth and death month
  socsec[,"bmonth" := as.numeric(substr(dob, 1, 2))]
  socsec[,"dmonth" := as.numeric(substr(dod, 1, 2))]
  ## birth and death dat
  socsec[,"bday" := as.numeric(substr(dob, 3, 4))]
  socsec[,"dday" := as.numeric(substr(dod, 3, 4))]
  ## now get census_age
  socsec[,"census_age" := ifelse(bmonth < 4,
                                 1940 - byear,
                                 1939 - byear)]

  ## filter to only include those with age info, that were born in 1940, and died after 1975.
  socsec <- socsec[socsec$census_age >= 0,]
  socsec <- socsec[socsec$dyear >= 1975,]
  socsec <- socsec[socsec$dyear <= 2005,]

  cat("Creating keys. \n")
  # create key
  socsec[,"tmp_key" := paste(lname, fname, census_age, sep = "_")]
  socsec[,"clean_key" := clean_key(tmp_key),]
  socsec[,"n_clean_key" := .N, by = clean_key]

  ## remove rows where fname, lname, or census age is missing.
  socsec <- na.omit(socsec, cols=c("fname", "lname", "census_age"))

  socsec <- socsec[,!"tmp_key"]
  return(socsec)

}

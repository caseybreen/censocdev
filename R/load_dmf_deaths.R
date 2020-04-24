#' Load a census file
#'
#' @param dmf a vector of file names where the social security death files live.
#' @return a dataframe with match key, age, and date of death
#' @import data.table
#' @export

load_dmf_deaths <- function(dmf_files){

  dmf_list<- list()
  for(i in 1:length(dmf_files)){
    dmf_file <- dmf_files[i]
    cat(paste0("Reading in file ", i, ".\n"))
    tt <- read_fwf(dmf_file,
                          fwf_widths(c(1,9, 20, 4, 15, 15, 1, 8, 8),
                                     col_names = c("mode", "ssn", "lname",
                                                   "name_suffix", "fname", "mname",
                                                   "vorpcode", "dod", "dob")))
    assign(paste0("dmf",i) , as.data.table(tt))
    dmf_list[[i]] <-eval(parse(text = paste0("dmf",i)))
    rm(tt)
  }

  dmf <- rbindlist(dmf_list)
  rm(dmf_list, dmf1, dmf2, dmf3)

  cat("Cleaning variables. \n")
  ## A. clean the dmf data
  ## shorten names by removing blank space at end
  dmf[,"lname_orginal_dmf" := lname]
  dmf[,"fname_orginal_dmf" := fname]
  dmf[,lname := gsub(pattern = "\\s*$",
                        replacement = "", x = lname)]
  dmf[,fname := gsub(pattern = "\\s*$",
                        replacement = "", x = fname)]

  ## now get birth and death year
  dmf[,"byear" := as.numeric(substr(dob, 5, 8))]
  dmf[,"dyear" := as.numeric(substr(dod, 5, 8))]
  ## birth and death month
  dmf[,"bmonth" := as.numeric(substr(dob, 1, 2))]
  dmf[,"dmonth" := as.numeric(substr(dod, 1, 2))]
  ## birth and death dat
  dmf[,"bday" := as.numeric(substr(dob, 3, 4))]
  dmf[,"dday" := as.numeric(substr(dod, 3, 4))]
  ## now get census_age
  dmf[,"census_age" := ifelse(bmonth < 4,
                                 1940 - byear,
                                 1939 - byear)]

  ## filter to only include those with age info, that were born in 1940, and died after 1975.
  dmf <- dmf[dmf$census_age >= 0,]
  dmf <- dmf[dmf$dyear >= 1975,]
  dmf <- dmf[dmf$dyear <= 2005,]

  cat("Creating keys. \n")
  # create key
  dmf[,"tmp_key" := paste(lname, fname, census_age, sep = "_")]
  dmf[,"clean_key" := clean_key(tmp_key),]
  dmf[,"n_clean_key" := .N, by = clean_key]

  ## remove rows where fname, lname, or census age is missing.
  dmf <- na.omit(dmf, cols=c("fname", "lname", "census_age"))

  dmf <- dmf[,!"tmp_key"]
  return(dmf)

}

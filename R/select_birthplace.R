#' Select place of birth
#'
#' @param numapp path to the NUMAPP files
#' @return NUMAPP data.frame with best bpl column and cycle dates
#' @keywords internal
#' @import data.table
#' @export

select_birthplace <- function(numapp = numapp,
                       crosswalk = "/censoc/data/crosswalks/bpl_crosswalk.csv") {
  
  
  numapp <- numapp[, c("ssn", "pob_state_country","pob_foreign_ind", "year_cycle", "month_cycle"), with=FALSE]
  
  ## Create foreign born dummy variable
  numapp[, bpl_foreign_flag := 0]
  numapp <- numapp[pob_foreign_ind == "f", bpl_foreign_flag := 1]
  
  ## Read in crosswalk
  crosswalk <- fread(crosswalk)
  
  ## Inner-Join Crosswalk; keep only certain years
  numapp <- merge(numapp, crosswalk, by.x = c("bpl_foreign_flag", "pob_state_country"), by.y=c("foreign_born", "ss_bpl"))
  
  ## Remove applications with 0 (no information) for sex
  applications <- nrow(numapp)
  numapp[bpl == " "] <- NA
  numapp <- na.omit(numapp, cols="bpl")
  removed_na <- applications - nrow(numapp)
  cat(removed_na, "applications removed with NA for bpl", "\n")
  
  ## Set missing values equal to 0. These will be selected last according to our selection process.
  for (col in c("year_cycle", "month_cycle")) numapp[is.na(get(col)), (col) := 0]
  
  ## Maybe should convert this to century months in the future?
  numapp[,"cycle_year_month" := year_cycle + (month_cycle/12)]
  numapp$cycle_year_month[is.na(numapp$cycle_year_month)] <- 0
  
  ## Number of different bpl per SSN
  numapp[, number_of_distinct_bpl:=uniqueN(bpl), by = ssn]
  
  ## Create flag (0 or 1 dichotomous var) for more than one birht place
  numapp[, bpl_multiple_flag := (ifelse(number_of_distinct_bpl > 1, 1, 0))]
  
  ## Select most recent bpl
  numapp <- numapp[numapp[, .I[which.max(cycle_year_month)], by=ssn]$V1]
  
  ## Recode BPL
  numapp[,"bpl_year_cycle" := year_cycle]
  numapp[,"bpl_month_cycle" := month_cycle]
  
  ## Recode originally missing years back to NA.
  numapp[year_cycle == 0, year_cycle := NA]
  numapp[month_cycle == 0, month_cycle := NA]
  
  numapp_bpl <- numapp[, c("ssn", "bpl", "bpl_year_cycle", "bpl_month_cycle", "bpl_multiple_flag", "bpl_foreign_flag" ), with=FALSE]
  
  return(numapp_bpl)
  
}

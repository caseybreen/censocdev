#' Select place of birth
#'
#' @param numapp path to the NUMAPP files
#' @return NUMAPP data.frame with best first name column and cycle dates
#' @keywords internal
#' @import data.table
#' @export

select_birthplace <- function(numapplication = numapp,

                              ssn_state_codes="/home/ipums/andrea-ipums/CenSoc/NUMI_GCBRTHST.txt",
                              ssn_country_codes="/home/ipums/andrea-ipums/CenSoc/nara_bpl.csv") {

  ssn_state_codes<-"/home/ipums/andrea-ipums/CenSoc/NUMI_GCBRTHST.txt"
  ssn_country_codes<-"/home/ipums/andrea-ipums/CenSoc/nara_bpl.csv"

  # Select variables from Num Application
  numapp <- SS5_complete[, c("ssn", "pob_state_country","pob_foreign_ind", "cycle_date"), with=FALSE]

  #Load country and state labels
  state_labels <- fread(ssn_state_codes)
  nara_bpl <- fread(ssn_country_codes)
  list_foreign <- as.character(unique(nara_bpl$Description))


  #Adding the labels for foreigners
  numapp[pob_foreign_ind=="f", birthplace := factor(pob_state_country,
                                          levels = unique(nara_bpl$unique_bpl),
                                          labels = nara_bpl$Description)]

  #Adding the labels for us-born SSN
  numapp[pob_foreign_ind=="", birthplace := factor(pob_state_country,
                                         levels = state_labels$Code,
                                         labels = state_labels$Description)]


   #########Choosing the best birth place
  #Rule 1: Using the place of birth closest to post 1940.
  numapp[,"cycle_year_month" := as.numeric(substr(cycle_date, 1, 6))]
  numapp[, earliest_cycle_month:= min(cycle_year_month, na.rm = TRUE), by=ssn]
  numapp[earliest_cycle_month<194004 , earliest_flag := NA]
  numapp[earliest_cycle_month>=194004 ,
      earliest_flag := (cycle_year_month-194004==min(cycle_year_month-194004, na.rm=TRUE)),
      by =ssn]


  numapp[earliest_flag==TRUE, bpl := as.character(birthplace)]
  numapp[bpl=="NA", bpl := NA]
  numapp[,check:=ifelse(earliest_flag==TRUE & !is.na(bpl),1,0)]
  numapp[,check:=max(check), by=ssn]
  numapp_check<- numapp[check==1 & earliest_flag==TRUE, ]

  #Rule 2: If place of birth closest to post 1940 is NA, then use the most common place of birth listed for each ssn
  numapp_nocheck<-numapp[check==0,]
  numapp_nocheck[, best_bpl2:=names(which.max(table(birthplace))), by=ssn ]
  numapp_nocheck[is.na(bpl), bpl:=best_bpl2]

  numapp_nocheck[, na_bpl:=all(is.na(birthplace)), by=ssn]
  numapp_nocheck[na_bpl==TRUE, bpl:=NA]


  #Binding both sets
  numapp_allcases <- rbind(numapp_check[,.(ssn, bpl)],numapp_nocheck[,.(ssn, bpl)])
  numapp_allcases[,foreign :=ifelse(bpl %in% list_foreign,1,0)]

  ######Removing duplicates
  numapp_birthplace <- numapp_allcases[!duplicated(numapp_allcases, by=c("ssn")),]


  return(numapp_birthplace)

}

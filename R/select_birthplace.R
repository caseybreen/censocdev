#' Select place of birth
#'
#' @param numapp path to the NUMAPP files
#' @return NUMAPP data.frame with best first name column and cycle dates
#' @keywords internal
#' @import data.table
#' @export

select_birthplace <- function(numapp = numapp,
                              nara_ipums_labels="/nobackup/90days/andreamg/nara_ipums_labels.csv",
                              nara_ipums_state_labels="/nobackup/90days/andreamg/nara_ipums_state_labels.csv",
                              nara_ipums_country_labels="/nobackup/90days/andreamg/nara_ipums_country_labels.csv") {

  ######Select variables from Num Application
  numapp <- numapp[, c("ssn", "pob_state_country","pob_foreign_ind", "cycle_date"), with=FALSE]

  ######Load country and state labels
  nara_ipums_bpl_labels <- fread(nara_ipums_labels)
  nara_ipums_country <- fread(nara_ipums_country_labels)
  nara_ipums_state <- fread(nara_ipums_state_labels)

  nara_ipums_bpl_labels <- as.data.table(nara_ipums_bpl_labels)
  nara_ipums_country <- as.data.table(nara_ipums_country)
  nara_ipums_state <- as.data.table(nara_ipums_state)

  list_foreign <- as.character(unique(nara_ipums_country$nara_country_code))
  list_state <- as.character(unique(nara_ipums_state$nara_state_code))
  list_nodescription_nara <- as.character(unique(nara_ipums_bpl_labels[grepl("_nolabel$", nara_code), label]))

  ######Changing some of the codes of pob_state_country to be comparable to IPUMS BPL
  #1. Countries
  numapp<- numapp[pob_foreign_ind=="f" & (pob_state_country=="VN"|pob_state_country=="VS"),pob_state_country:="VM"]
  numapp<- numapp[pob_foreign_ind=="f" & pob_state_country=="CF",pob_state_country:="CG"]
  numapp<- numapp[pob_foreign_ind=="f" & (pob_state_country=="GS"|pob_state_country=="GN"),pob_state_country:="KR"]
  numapp<- numapp[pob_foreign_ind=="f" & pob_state_country=="YQ",pob_state_country:="JA"]
  numapp<- numapp[pob_foreign_ind=="f" & pob_state_country=="HI",pob_state_country:="EI"]
  numapp<- numapp[pob_foreign_ind=="f" & pob_state_country=="YM",pob_state_country:="YE"]
  numapp<- numapp[pob_foreign_ind=="f" & pob_state_country=="RH",pob_state_country:="ZI"]
  numapp<- numapp[pob_foreign_ind=="f" & pob_state_country=="DM",pob_state_country:="BN"]
  numapp<- numapp[pob_foreign_ind=="f" & pob_state_country=="SK",pob_state_country:="IN"]
  numapp<- numapp[pob_foreign_ind=="f" & pob_state_country=="SR",pob_state_country:="RI"]
  numapp<- numapp[pob_foreign_ind=="f" & pob_state_country=="SS",pob_state_country:="WI"]
  numapp<- numapp[pob_foreign_ind=="f" & pob_state_country=="MW",pob_state_country:="MJ"]
  numapp<- numapp[pob_foreign_ind=="f" & pob_state_country=="FT",pob_state_country:="DJ"]
  numapp<- numapp[pob_foreign_ind=="f" & pob_state_country=="PT",pob_state_country:="TT"]
  numapp<- numapp[pob_foreign_ind=="f" & pob_state_country=="SQ",pob_state_country:="HO"]
  numapp<- numapp[pob_foreign_ind=="f" & pob_state_country=="TC",pob_state_country:="AE"]
  numapp<- numapp[pob_foreign_ind=="f" & pob_state_country=="FK",pob_state_country:="FA"]
  numapp<- numapp[pob_foreign_ind=="f" & pob_state_country=="UR",pob_state_country:="RS"]

  #2. States: adding "_USA" to codes for US states
  numapp <- numapp[pob_state_country!="" & pob_foreign_ind=="", pob_state_country:=paste0(pob_state_country,"_USA")]

  ##Changing the codes to ensure that they are unique:
  # When the label=nara_code (for now we just add an indicator to the nara_code to remember that these codes are incomplete)
  numapp <- numapp[pob_state_country %in% list_nodescription_nara &
                     pob_foreign_ind=="f", pob_state_country:=paste0(pob_state_country,"_nolabel")]


  ######Additional variables
  #Extracting the year of each application
  numapp[,year_cycle := as.numeric(substr(cycle_date, 1, 4))]
  numapp[,month_cycle := as.numeric(substr(cycle_date, 5, 6))]
  #Set missing values equal to 0. These will be selected last according to our selection process.
  for (col in c("year_cycle", "month_cycle")) numapp[is.na(get(col)), (col) := 0]
  #Maybe should convert this to century months in the future?
  numapp[,"cycle_year_month" := year_cycle + (month_cycle/12)]

  #Number of applications
  numapp[ , n_app:=.N, by=ssn]
  #Number of different places of birth
  numapp[pob_state_country=="", pob_state_country:=NA]
  numapp[ , n_bpl:=uniqueN(pob_state_country), by=ssn]
  numapp[ , bpl_multiple_flag:=ifelse(n_bpl>1,1,0)]
  #Foreigners
  numapp[pob_foreign_ind=="" , foreign:=0]
  numapp[pob_foreign_ind=="f", foreign:=1]
  #Change in citizenship
  numapp[, bpl_change_flag:=max(foreign)-min(foreign), by=ssn]

  #Number of times that all applications have an NA place of birth
  numapp[, na_bpl:=all(is.na(pob_state_country)), by=ssn]


  #########Choosing the best birth place
  #Rule 1: If there is only one application, use that birthplace and cycle_year
  numapp_1 <- numapp[n_app==1 |na_bpl==TRUE,]
  numapp_1[, bpl:=pob_state_country]
  numapp_1[, bpl_year:=year_cycle]
  numapp_1[, bpl_month:=month_cycle]

  #Rule 2: Using the lastest application
  numapp_2 <- numapp[n_app>1,]
  numapp_2 <- numapp_2[, last_cycle:=(cycle_year_month==max(cycle_year_month)), by=ssn]

  numapp_2[last_cycle==TRUE, bpl := pob_state_country]
  numapp_2[last_cycle==TRUE, bpl_year := year_cycle]
  numapp_2[last_cycle==TRUE, bpl_month := month_cycle]

  #Flag applications that do have a place of birth stated in their LAST cycle
  numapp_2[,flag:=max(ifelse(last_cycle==TRUE & !is.na(bpl),1,0)), by=ssn]
  numapp_2a<- numapp_2[flag==1 & last_cycle==TRUE, ]

  #Rule 3: If place of birth closest to post 1940 is NA, then use the most common place of birth listed for each ssn
  numapp_3<-numapp_2[flag==0,]
  numapp_3[, bpl:=names(which.max(table(pob_state_country))), by=ssn ]
  numapp_3[, bpl_year:=year_cycle]
  numapp_3[, bpl_month:=month_cycle]

  ######Binding both sets
  numapp_allcases <- rbind(numapp_1[,.(ssn, bpl, bpl_year, bpl_month, bpl_multiple_flag, bpl_change_flag)],
                           numapp_2a[,.(ssn, bpl, bpl_year, bpl_month, bpl_multiple_flag, bpl_change_flag)],
                           numapp_3[,.(ssn, bpl, bpl_year, bpl_month, bpl_multiple_flag, bpl_change_flag) ])
  numapp_allcases[,foreign :=ifelse(bpl %in% list_foreign,1,0)]

  ######Removing duplicates
  numapp_birthplace <- numapp_allcases[!duplicated(numapp_allcases, by=c("ssn")),]

  #Adding the labels for foreigners
  #numapp_birthplace <- numapp_birthplace[!duplicated(numapp_birthplace, by=c("ssn")),]
  numapp_birthplace <- left_join(numapp_birthplace,nara_ipums_bpl_labels, by=c("bpl"="nara_code"))
  numapp_birthplace <- as.data.table(numapp_birthplace)
  numapp_birthplace <- numapp_birthplace[is.na(bpl), ipums_code:=NA]
  numapp_birthplace <- numapp_birthplace[is.na(bpl), label:=NA]
  numapp_birthplace <-numapp_birthplace[ , .(ssn, bpl, bpl_year, bpl_month, bpl_multiple_flag, bpl_change_flag,foreign.x,ipums_code,label)]
  numapp_birthplace <-numapp_birthplace[ , .(ssn, bpl = ipums_code, bpl_cyear = bpl_year, bpl_cmonth = bpl_month, bpl_flag = bpl_multiple_flag, foreign = foreign.x)]

  return(numapp_birthplace)

}

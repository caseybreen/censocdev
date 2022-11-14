#########################################
# created by UGUR YILDIRIM (2020-08-16) #
# revised by WON-TAK JOO (2022-11-10)   #
#########################################

split_by_bpl <- function(out_path, data, split_var, cols_to_keep=NULL) {
  
  # Require libraries
  require(data.table)
  require(readr)
  
  # Set working directory to out_path (create out_path if doesn't exist)
  dir.create(out_path)
  setwd(out_path)
  
  # Deal with cases with missing birthplace
  if (split_var == "BPL") {
    data$BPL[is.na(data$BPL)] <- 99999
  } else {
    data$bpl[is.na(data$bpl)] <- 99999
  }
  
  # Split by birthplace
  data_split <- split(data, data[[split_var]])
  for(bpl in data_split) {
    temp1 <- as.data.frame(bpl)
    temp2 <- paste0("bpl_", as.character(temp1[[split_var]][1]))
    #assign(temp2, temp1)
    #save(list=temp2, file=paste0(temp2, ".rda"))
    write_csv(temp1, paste0(temp2, ".csv"))
    rm(temp1, temp2)
  }
  
  # Clean up after yourself
  rm(list = ls())
  
}

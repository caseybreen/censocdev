#########################################
# created by UGUR YILDIRIM (2020-09-30) #
# revised by WON-TAK JOO (2022-11-10)   #
#########################################

# Define %>%
`%>%` <- magrittr::`%>%`

# Define preprocess_A
#' @export
preprocess_A <- function(file_A, fname_var, lname_var, time_var, id_var) {
  # Keep fname_var, lname_var, time_var, id_var
  file_A <- subset(file_A, select=c(fname_var, lname_var, time_var, id_var))
  colnames(file_A) <- c("fname", "lname", "age", "id_A")
  
  # Order rows based on fname_var, lname_var, time_var
  file_A <- file_A[order(fname, lname, age), ]
  
  # Create file_A flag
  file_A$file_A <- 1

  # Create ID_A column
  file_A$ID_A <- 1:nrow(file_A)

  # Remove duplicate rows
  N <- file_A[,.N, by=c("fname", "lname", "age")]
  file_A <- merge(file_A, N, by=c("fname", "lname", "age"), all.x = TRUE)
  file_A <- subset(file_A, N == 1)
  file_A$N <- NULL
  rm(N)
  
  # Remove rows with NA
  file_A <- file_A[!is.na(fname) & !is.na(lname)]

  # Return file_A
  return(file_A)
}
# NOTES
# 1. Double check that the ordering given by order(...) is unique.
# 2. Add code chunk to check that id_var uniquely identifies rows.

# Define preprocess_B
#' @export
preprocess_B <- function(file_B, fname_var, lname_var, time_var, id_var) {
  # Keep fname_var, lname_var, time_var, id_var
  file_B <- subset(file_B, select=c(fname_var, lname_var, time_var, id_var))
  colnames(file_B) <- c("fname", "lname", "age", "id_B")
  
  # Order rows based on fname_var, lname_var, time_var
  file_B <- file_B[order(fname, lname, age), ]
  
  # Create file_B flag
  file_B$file_B <- 1

  # Create ID_B column
  file_B$ID_B <- 1:nrow(file_B)

  # Return file_B
  return(file_B)
}
# NOTES
# 1. Double check that the ordering given by order(...) is unique.
# 2. Add code chunk to check that id_var uniquely identifies rows.

# Define append_A_to_B
#' @export
append_A_to_B <- function(file_A, file_B, uniqueband_file=2, backward=FALSE) {
  # Append B to A
  file_AB <- rbind(file_B, file_A, fill=TRUE)

  # Fill in missing file flags
  file_AB$file_B <- ifelse(is.na(file_AB$file_B), 0, file_AB$file_B)
  file_AB$file_A <- ifelse(is.na(file_AB$file_A), 0, file_AB$file_A)

  # Initialize matched_at_A, exactmatch1
  file_AB$matched_at_A <- NA
  file_AB$exactmatch1  <- 0

  # Create count_A, count_B
  if (backward) {
    sum <- file_AB[,.(count_A=sum(file_B), count_B=sum(file_A)), by=c("fname", "lname", "age")]
    file_AB <- merge(file_AB, sum, by=c("fname", "lname", "age"), all.x = TRUE)
    rm(sum)
  } else {
    sum <- file_AB[,.(count_A=sum(file_A), count_B=sum(file_B)), by=c("fname", "lname", "age")]
    file_AB <- merge(file_AB, sum, by=c("fname", "lname", "age"), all.x = TRUE)
    rm(sum)
  }

  # Conservative ABE
  if (backward) {
    file_AB <- file_AB[order(file_B, fname, lname, age)]
    for (i in 1:uniqueband_file) {
      file_AB$temp <- 0
      N <- file_AB[,.(idx=seq_len(.N), tot=.N), by=c("file_B", "fname", "lname")]
      file_AB$idx <- N$idx
      file_AB$tot <- N$tot
      rm(N)
      file_AB$age_lag  <- data.table::shift(file_AB$age, n=1, fill=NA, type="lag")
      file_AB$age_lead <- data.table::shift(file_AB$age, n=1, fill=NA, type="lead")
      file_AB$temp <- ifelse((file_AB$age - i <= file_AB$age_lag  & file_AB$idx > 1) |
                             (file_AB$age + i >= file_AB$age_lead & file_AB$idx < file_AB$tot), 1, file_AB$temp)
      file_AB[[paste0("uniquestub_file", i)]] <- 1 - file_AB$temp
      file_AB$temp <- NULL
    }
    file_AB$idx <- NULL
    file_AB$tot <- NULL
    file_AB$age_lag <- NULL
    file_AB$age_lead <- NULL
  } else {
    file_AB <- file_AB[order(file_A, fname, lname, age)]
    for (i in 1:uniqueband_file) {
      file_AB$temp <- 0
      N <- file_AB[,.(idx=seq_len(.N), tot=.N), by=c("file_A", "fname", "lname")]
      file_AB$idx <- N$idx
      file_AB$tot <- N$tot
      rm(N)
      file_AB$age_lag  <- data.table::shift(file_AB$age, n=1, fill=NA, type="lag")
      file_AB$age_lead <- data.table::shift(file_AB$age, n=1, fill=NA, type="lead")
      file_AB$temp <- ifelse((file_AB$age - i <= file_AB$age_lag  & file_AB$idx > 1) |
                             (file_AB$age + i >= file_AB$age_lead & file_AB$idx < file_AB$tot), 1, file_AB$temp)
      file_AB[[paste0("uniquestub_file", i)]] <- 1 - file_AB$temp
      file_AB$temp <- NULL
    }
    file_AB$idx <- NULL
    file_AB$tot <- NULL
    file_AB$age_lag <- NULL
    file_AB$age_lead <- NULL
  }

  # Update exactmatch1
  file_AB$exactmatch1 <- ifelse(file_AB$count_A == 1 & file_AB$count_B == 1, 1, file_AB$exactmatch1)

  # Drop rows in A that correspond to multiple rows in B
  if (backward) {
    file_AB$drop <- ifelse(file_AB$count_A > 1 & file_AB$count_B == 1 & file_AB$file_A == 1, 1, 0)
  } else {
    file_AB$drop <- ifelse(file_AB$count_B > 1 & file_AB$count_A == 1 & file_AB$file_A == 1, 1, 0)
  }
  file_AB         <- subset(file_AB, drop == 0)
  file_AB$drop    <- NULL
  file_AB$count_A <- NULL
  file_AB$count_B <- NULL

  # Update matched_at_A
  file_AB$matched_at_A <- ifelse(file_AB$exactmatch1 == 1 & file_AB$file_A == 1, 0, file_AB$matched_at_A)

  # Conservative ABE
  sum <- file_AB[,.(uniquestub_match0=sum(file_B)), by=c("fname", "lname", "age")]
  file_AB <- merge(file_AB, sum, by=c("fname", "lname", "age"), all.x = TRUE)
  rm(sum)
  
  # Return file_AB
  return(file_AB)
}
# NOTES
# 1. Add a timediff parameter to be able to work with alternative time variables such as age.

# Define find_nonexact_matches
#' @export
find_nonexact_matches <- function(file_AB, timeband=2, uniqueband_match=2, backward=FALSE) {
  # Check if user asked for non-exact matches
  if (timeband > 0) {

    # Initialize already
    file_AB$already <- file_AB$exactmatch1

    # Loop over timeband values
    for (i in 1:timeband) {
      
      # Create age_mi, age_pi
      file_AB[[paste0("age_m", i)]] <- ifelse(file_AB$file_B == 1, file_AB$age, file_AB$age - i)
      file_AB[[paste0("age_p", i)]] <- ifelse(file_AB$file_B == 1, file_AB$age, file_AB$age + i)
      
      # Create unmatched_A
      file_AB$unmatched_A <- ifelse(file_AB$file_A == 1 & file_AB$already == 0, 1, 0)
      
      # Search -i
      age_mi <- paste0("age_m", i)
      sum <- file_AB[,.(mcount_A=sum(unmatched_A), mcount_B=sum(file_B), existing_matches=sum(already)), by=c("fname", "lname", age_mi)]
      file_AB <- merge(file_AB, sum, by=c("fname", "lname", age_mi), all.x = TRUE)
      rm(sum)

      # Create exactmatch1_mi
      file_AB[[paste0("exactmatch1_m", i)]] <- ifelse(file_AB$mcount_A == 1 &
                                                      file_AB$mcount_B == 1 &
                                                      file_AB$existing_matches == 0, 1, NA)
      
      # Drop rows in A-i that correspond to multiple rows in B
      file_AB$drop <- ifelse(file_AB$mcount_B > 1  &
                             file_AB$mcount_A == 1 &
                             file_AB$file_A == 1   &
                             file_AB$existing_matches == 0, 1, 0)
      file_AB      <- subset(file_AB, drop == 0)
      file_AB$drop <- NULL
      file_AB$existing_matches <- NULL
      
      # Search +i
      fname    <- "fname"
      lname    <- "lname"
      age_pi <- paste0("age_p", i)
      sum <- file_AB[,.(pcount_A=sum(unmatched_A), pcount_B=sum(file_B), existing_matches=sum(already)), by=c("fname", "lname", age_pi)]
      file_AB <- merge(file_AB, sum, by=c("fname", "lname", age_pi), all.x = TRUE)
      rm(sum)

      # Create exactmatch1_pi
      file_AB[[paste0("exactmatch1_p", i)]] <- ifelse(file_AB$pcount_A == 1 &
                                                        file_AB$pcount_B == 1 &
                                                        file_AB$existing_matches == 0, 1, NA)
      
      # Drop rows in A+i that correspond to multiple rows in B
      file_AB$drop <- ifelse(file_AB$pcount_B > 1  &
                               file_AB$pcount_A == 1 &
                               file_AB$file_A == 1   &
                               file_AB$existing_matches == 0, 1, 0)
      file_AB      <- subset(file_AB, drop == 0)
      file_AB$drop <- NULL
      file_AB$existing_matches <- NULL
      
      # Clean up exactmatch1_m, exactmatch1_p, matched_at_A
      file_AB[[paste0("exactmatch1_m", i)]] <- ifelse(is.na(file_AB[[paste0("exactmatch1_m", i)]]),
                                                      0,
                                                      file_AB[[paste0("exactmatch1_m", i)]])
      file_AB[[paste0("exactmatch1_p", i)]] <- ifelse(is.na(file_AB[[paste0("exactmatch1_p", i)]]),
                                                      0,
                                                      file_AB[[paste0("exactmatch1_p", i)]])
      file_AB$matched_at_A <- ifelse(file_AB[[paste0("exactmatch1_p", i)]] == 1 &
                                       file_AB[[paste0("exactmatch1_m", i)]] == 0 &
                                       file_AB$already != 1 &
                                       file_AB$file_A == 1, i, file_AB$matched_at_A)
      file_AB$matched_at_A <- ifelse(file_AB[[paste0("exactmatch1_p", i)]] == 0 &
                                       file_AB[[paste0("exactmatch1_m", i)]] == 1 &
                                       file_AB$already != 1 &
                                       file_AB$file_A == 1, -i, file_AB$matched_at_A)
      
      # Update already
      file_AB$already <- ifelse(file_AB[[paste0("exactmatch1_p", i)]] == 1 &
                                  file_AB[[paste0("exactmatch1_m", i)]] == 0, 1, file_AB$already)
      file_AB$already <- ifelse(file_AB[[paste0("exactmatch1_p", i)]] == 0 &
                                  file_AB[[paste0("exactmatch1_m", i)]] == 1, 1, file_AB$already)
      
      # Conservative ABE
      file_AB[[paste0("uniquestub_match", i)]] <- file_AB$pcount_B + file_AB$mcount_B
      file_AB[[paste0("uniquestub_match", i)]] <- ifelse(file_AB$file_A != 1,
                                                         NA,
                                                         file_AB[[paste0("uniquestub_match", i)]])
      
      # Drop temporary columns
      file_AB$pcount_B    <- NULL
      file_AB$pcount_A    <- NULL
      file_AB$mcount_B    <- NULL
      file_AB$mcount_A    <- NULL
      file_AB$unmatched_A <- NULL
    }
  }
  
  # Drop unmatched in A
  file_AB$drop <- ifelse(is.na(file_AB$matched_at_A) & file_AB$file_A == 1, 1, 0)
  file_AB      <- subset(file_AB, drop == 0)
  file_AB$drop <- NULL
  
  # Conservative ABE
  if (uniqueband_match > timeband) {
    start <- timeband + 1
    for (i in start:uniqueband_match) {
      file_AB[[paste0("uage_m", i)]] <- file_AB$age - i
      file_AB[[paste0("uage_p", i)]] <- file_AB$age + i
      file_AB[[paste0("uage_m", i)]] <- ifelse(file_AB$file_B == 1,
                                               file_AB$age,
                                               file_AB[[paste0("uage_m", i)]])
      file_AB[[paste0("uage_p", i)]] <- ifelse(file_AB$file_B == 1,
                                               file_AB$age,
                                               file_AB[[paste0("uage_p", i)]])
      uage_mi <- paste0("uage_m", i)
      sum <- file_AB[,.(unique_m=sum(file_B)), by=c("fname", "lname", uage_mi)]
      file_AB <- merge(file_AB, sum, by=c("fname", "lname", uage_mi), all.x = TRUE)
      rm(sum)
      
      uage_pi <- paste0("uage_p", i)
      sum <- file_AB[,.(unique_m=sum(file_B)), by=c("fname", "lname", uage_pi)]
      file_AB <- merge(file_AB, sum, by=c("fname", "lname", uage_pi), all.x = TRUE)
      rm(sum)
      
      file_AB[[paste0("uniquestub_match", i)]] <- file_AB$unique_m + file_AB$unique_p
      file_AB[[paste0("uniquestub_match", i)]] <- ifelse(file_AB$file_A != 1,
                                                         NA,
                                                         file_AB[[paste0("uniquestub_match", i)]])
      file_AB[[paste0("uage_m", i)]] <- NULL
      file_AB[[paste0("uage_p", i)]] <- NULL
      file_AB$unique_m <- NULL
      file_AB$unique_p <- NULL
    }
  }
  # NOTES
  # 1. This chunk is not tested yet.

  # Conservative ABE
  for (i in 1:uniqueband_match) {
    j <- i - 1
    file_AB[[paste0("uniquestub_match", i)]] <- file_AB[[paste0("uniquestub_match", i)]] +
                                                file_AB[[paste0("uniquestub_match", j)]]
  }
  for (i in 1:uniqueband_match) {
    file_AB[[paste0("uniquestub_match", i)]] <-
      ifelse(file_AB$file_A == 1,
             file_AB[[paste0("uniquestub_match", i)]] <= 1,
             file_AB[[paste0("uniquestub_match", i)]])
  }
  file_AB$uniquestub_match0 <- NULL

  # Drop unmatched in A
  file_AB$drop <- ifelse(is.na(file_AB$matched_at_A) & file_AB$file_A == 1, 1, 0)
  file_AB      <- subset(file_AB, drop == 0)
  file_AB$drop <- NULL

  # Create timevar_keep1
  file_AB$timevar_keep1 <- ifelse(file_AB$file_A == 1, file_AB$age + file_AB$matched_at_A, file_AB$age)

  # Make sure only two individuals per matched pair
  sum <- file_AB[,.(count_A=sum(file_A), count_B=sum(file_B)), by=c("fname", "lname", "timevar_keep1")]
  file_AB <- merge(file_AB, sum, by=c("fname", "lname", "timevar_keep1"), all.x = TRUE)
  rm(sum)
  
  file_AB <- subset(file_AB, count_A == 1 & count_B == 1)
  file_AB$count_A <- NULL
  file_AB$count_B <- NULL

  # Rename columns if backward direction (list updated for conservative ABE)
  if (backward) {
    colnames(file_AB) <- c("fname", "lname", "timevar_keep2", "age_p2", "age_m2", "age_p1", "age_m1",
                           "age", "id_A", "file_A", "ID_A", "id_B", "file_B", "ID_B", "matched_at_B",
                           "exactmatch2", "uniquestub_file1", "uniquestub_file2", "already",
                           "exactmatch2_m1", "exactmatch2_p1", "uniquestub_match1",
                           "exactmatch2_m2", "exactmatch2_p2", "uniquestub_match2")
  }

  # Return file_AB
  return(file_AB)
}
# NOTES
# 1. Count number of matches at the end.

# Define fix_ids
#' @export
fix_ids <- function(file_AB_processed, timevar_keep, uniqueband_file=2, uniqueband_match=2, backward=FALSE) {
  # Order rows based on fname, lname, timevar_keep, file_A
  file_AB_processed <- file_AB_processed[order(file_AB_processed$fname,           file_AB_processed$lname,
                                               file_AB_processed[[timevar_keep]], file_AB_processed$file_A), ]

  # Fix ID's
  file_AB_processed$ID_A <- ifelse(file_AB_processed$file_B == 1,
                                   data.table::shift(file_AB_processed$ID_A, n=1, fill=NA, type="lead"),
                                   file_AB_processed$ID_A)
  file_AB_processed$ID_B <- ifelse(file_AB_processed$file_A == 1,
                                   data.table::shift(file_AB_processed$ID_B, n=1, fill=NA, type="lag"),
                                   file_AB_processed$ID_B)

  # Conservative ABE
  if (backward) {
    for (i in 1:uniqueband_match) {
      file_AB_processed$temp <- file_AB_processed[[paste0("uniquestub_match", i)]]
      min <- file_AB_processed[,.(min=min(temp, na.rm=TRUE)), by="ID_B"]
      file_AB_processed <- merge(file_AB_processed, min, by="ID_B", all.x = TRUE)
      rm(min)
      file_AB_processed[[paste0("uniquestub_match", i)]] = file_AB_processed$min
      file_AB_processed$temp <- NULL
      file_AB_processed$min <- NULL
    }
    for (i in 1:uniqueband_file) {
      file_AB_processed$temp <- file_AB_processed[[paste0("uniquestub_file", i)]]
      min <- file_AB_processed[,.(min=min(temp, na.rm=TRUE)), by="ID_B"]
      file_AB_processed <- merge(file_AB_processed, min, by="ID_B", all.x = TRUE)
      rm(min)
      file_AB_processed[[paste0("uniquestub_file", i)]] = file_AB_processed$min
      file_AB_processed$temp <- NULL
      file_AB_processed$min <- NULL
    }
  } else {
    for (i in 1:uniqueband_match) {
      file_AB_processed$temp <- file_AB_processed[[paste0("uniquestub_match", i)]]
      min <- file_AB_processed[,.(min=min(temp, na.rm=TRUE)), by="ID_A"]
      file_AB_processed <- merge(file_AB_processed, min, by="ID_A", all.x = TRUE)
      rm(min)
      file_AB_processed[[paste0("uniquestub_match", i, "A")]] = file_AB_processed$min
      file_AB_processed[[paste0("uniquestub_match", i)]] <- NULL
      file_AB_processed$temp <- NULL
      file_AB_processed$min <- NULL
    }
    for (i in 1:uniqueband_file) {
      file_AB_processed$temp <- file_AB_processed[[paste0("uniquestub_file", i)]]
      min <- file_AB_processed[,.(min=min(temp, na.rm=TRUE)), by="ID_A"]
      file_AB_processed <- merge(file_AB_processed, min, by="ID_A", all.x = TRUE)
      rm(min)
      file_AB_processed[[paste0("uniquestub_file", i, "A")]] = file_AB_processed$min
      file_AB_processed[[paste0("uniquestub_file", i)]] <- NULL
      file_AB_processed$temp <- NULL
      file_AB_processed$min <- NULL
    }
  }

  # Return file_AB_processed
  return(file_AB_processed)
}
# NOTES
# 1. Add code chunk to check whether either direction has 0 rows.

#' Match records
#'
#' This function matches records in dataset A to records in dataset B
#' using the ABE method. The matched dataset includes both standard
#' and conservative ABE matches.
#'
#' @param file_A           Dataset A
#' @param fname_var_A      First name column in dataset A
#' @param lname_var_A      Last name column in dataset A
#' @param time_var_A       Time column in dataset A
#' @param id_var_A         ID column in dataset A
#' @param vars_to_keep_A   Other columns to be kept from dataset A
#' @param file_B           Dataset B
#' @param fname_var_B      First name column in dataset B
#' @param lname_var_B      Last name column in dataset B
#' @param time_var_B       Time column in dataset B
#' @param id_var_B         ID column in dataset B
#' @param vars_to_keep_B   Other columns to be kept from dataset B
#' @param out_path         Path to the directory where the output file will be saved
#' @param out_file_name    Output filename
#' @param timeband         Time band used when searching for nonexact matches
#' @param uniqueband_file  Uniqueness band used for conservative ABE (within)
#' @param uniqueband_match Uniqueness band used for conservative ABE (between)
#' @return                 NULL
#' @export
match_records <- function(file_A, fname_var_A, lname_var_A, time_var_A, id_var_A, vars_to_keep_A,
                          file_B, fname_var_B, lname_var_B, time_var_B, id_var_B, vars_to_keep_B,
                          out_path, out_file_name, timeband=2, uniqueband_file=2, uniqueband_match=2) {
  # Forward pass
  file_A_forward            <- preprocess_A(file_A, fname_var_A, lname_var_A, time_var_A, id_var_A)
  file_B_forward            <- preprocess_B(file_B, fname_var_B, lname_var_B, time_var_B, id_var_B)
  file_AB_forward           <- append_A_to_B(file_A_forward, file_B_forward, uniqueband_file, FALSE)
  file_AB_processed_forward <- find_nonexact_matches(file_AB_forward, timeband, uniqueband_match, FALSE)

  # Backward pass
  file_A_backward            <- preprocess_A(file_B, fname_var_B, lname_var_B, time_var_B, id_var_B)
  file_B_backward            <- preprocess_B(file_A, fname_var_A, lname_var_A, time_var_A, id_var_A)
  file_AB_backward           <- append_A_to_B(file_A_backward, file_B_backward, uniqueband_file, TRUE)
  file_AB_processed_backward <- find_nonexact_matches(file_AB_backward, timeband, uniqueband_match, TRUE)

  # Fix ID's
  forward  <- fix_ids(file_AB_processed_forward,  "timevar_keep1", uniqueband_file, uniqueband_match, FALSE)
  backward <- fix_ids(file_AB_processed_backward, "timevar_keep2", uniqueband_file, uniqueband_match, TRUE)

  # Keep only necessary columns (list updated for conservative ABE)
  forward  <- forward[, c("ID_A", "ID_B", "file_A",           "matched_at_A",
                          "uniquestub_match1A", "uniquestub_file1A", "uniquestub_match2A", "uniquestub_file2A")]
  backward <- backward[,c("ID_A", "ID_B", "file_A", "file_B", "matched_at_B",
                          "uniquestub_match1",  "uniquestub_file1",  "uniquestub_match2",  "uniquestub_file2")]

  # Join backward to forward
  final <- merge(backward, forward, by = c("ID_A", "ID_B", "file_A"))
  # NOTES
  # 1. Add code chunk to check whether final has 0 rows.

  # Split final to A and B and create timediff
  final_A <- subset(final, file_A == 1)
  final_B <- subset(final, file_B == 1)
  final_A$timediff_A <- final_A$matched_at_A
  final_B$timediff_B <- final_B$matched_at_B
  final_A$matched_at_A <- NULL
  final_A$matched_at_B <- NULL
  final_B$matched_at_A <- NULL
  final_B$matched_at_B <- NULL
  final_A$file_A       <- NULL
  final_A$file_B       <- NULL
  final_B$file_A       <- NULL
  final_B$file_B       <- NULL

  # Merge back A
  file_A           <- subset(file_A, select=c(fname_var_A, lname_var_A, time_var_A, id_var_A, vars_to_keep_A))
  file_A           <- file_A[order(get(fname_var_A), get(lname_var_A), get(time_var_A)), ]
  file_A$ID_A      <- 1:nrow(file_A)
  file_A           <- subset(file_A, select=c("ID_A", id_var_A, fname_var_A, lname_var_A, time_var_A, vars_to_keep_A))
  colnames(file_A) <- c("ID_A", "id_A", fname_var_A, lname_var_A, paste0(c(time_var_A, vars_to_keep_A), "_A"))
  file_A           <- merge(file_A, final_A, by = "ID_A")

  # Merge back B
  file_B           <- subset(file_B, select=c(fname_var_B, lname_var_B, time_var_B, id_var_B, vars_to_keep_B))
  file_B           <- file_B[order(get(fname_var_B), get(lname_var_B), get(time_var_B)), ]
  file_B$ID_B      <- 1:nrow(file_B)
  file_B           <- subset(file_B, select=c("ID_B", id_var_B, fname_var_B, lname_var_B, time_var_B, vars_to_keep_B))
  colnames(file_B) <- c("ID_B", "id_B", fname_var_B, lname_var_B, paste0(c(time_var_B, vars_to_keep_B), "_B"))
  file_B           <- merge(file_B, final_B, by = "ID_B")
  
  # Merge A to B
  file_A           <- subset(file_A, select=c("ID_A", "id_A", fname_var_A, lname_var_A, paste0(c(time_var_A, vars_to_keep_A), "_A"), "timediff_A",
                                "uniquestub_match1A", "uniquestub_file1A", "uniquestub_match2A", "uniquestub_file2A",
                                "uniquestub_match1",  "uniquestub_file1",  "uniquestub_match2",  "uniquestub_file2"))
  file_B           <- subset(file_B, select=c("ID_A", "id_B",                           paste0(c(time_var_B, vars_to_keep_B), "_B"), "timediff_B"))
  res              <- merge(file_B, file_A, by = "ID_A")
  res$ID_A         <- NULL

  # Conservative ABE
  for (i in 1:uniqueband_match) {
    res[[paste0("uniquestub_match", i)]] <-
      ifelse(res[[paste0("uniquestub_match", i, "A")]] < res[[paste0("uniquestub_match", i)]],
             res[[paste0("uniquestub_match", i, "A")]],
             res[[paste0("uniquestub_match", i)]])
    res[[paste0("uniquestub_match", i, "A")]] <- NULL
  }
  for (i in 1:uniqueband_file) {
    res[[paste0("uniquestub_file", i)]] <-
      ifelse(res[[paste0("uniquestub_file", i, "A")]] < res[[paste0("uniquestub_file", i)]],
             res[[paste0("uniquestub_file", i, "A")]],
             res[[paste0("uniquestub_file", i)]])
    res[[paste0("uniquestub_file", i, "A")]] <- NULL
  }

  # Save dataset
  #dir.create(out_path, showWarnings = FALSE)
  path_to_out_file <- paste(out_path, out_file_name, sep="/")
  fwrite(res, path_to_out_file)

  # Clean up after yourself
  rm(list = ls())

}

#########################################
# created by UGUR YILDIRIM (2020-05-01) #
# revised by WON-TAK JOO (2022-11-10)   #
#########################################
#   previously converted male_nicknames.csv had some problems in some nicknames (e.g., marshall)
#   I took all the nicknames again from Abramitzky's website (https://ranabr.people.stanford.edu/)
#   previously converted titles.csv was used since I had no access to the original titles.txt

# Load libraries
library(readr)
library(tidyverse)
library(here) #set project to censocdev

# Paths
in_path <- here("codebase/02_create_censoc/titles-and-nicknames")

# Parse titles file
#temp1 <- read_file(paste(in_path, "titles.txt", sep="/"))
#temp2 <- str_remove_all(temp1, "///\r\n\t+")
#temp3 <- gsub("\\s+", " ", temp2)
#temp4 <- unlist(strsplit(temp3, " "))
#titles <- data.frame(matrix(unlist(temp4), nrow=length(temp4), byrow=T))
#colnames(titles) <- "title"
#rm(temp1, temp2, temp3, temp4)

# Parse male/female/old nicknames files
parse_row <- function(row) return(unlist(strsplit(str_remove_all(row, "\""), ",")))

temp1 <- read_file(paste(in_path, "male_nicknames.txt", sep="/"))
temp2 <- unlist(strsplit(temp1, " _n "))
temp3 <- lapply(temp2[2:length(temp2)], parse_row)
male_nicknames <- data.frame(matrix(unlist(temp3), nrow=length(temp3), byrow=T))
colnames(male_nicknames) <- parse_row(temp2[1])
rm(temp1, temp2, temp3)

temp1 <- read_file(paste(in_path, "female_nicknames.txt", sep="/"))
temp2 <- unlist(strsplit(temp1, " _n "))
temp3 <- lapply(temp2[2:length(temp2)], parse_row)
female_nicknames <- data.frame(matrix(unlist(temp3), nrow=length(temp3), byrow=T))
colnames(female_nicknames) <- parse_row(temp2[1])
rm(temp1, temp2, temp3)

temp1 <- read_file(paste(in_path, "old_nicknames.txt", sep="/"))
temp2 <- unlist(strsplit(temp1, " _n "))
temp3 <- lapply(temp2[2:length(temp2)], parse_row)
old_nicknames <- data.frame(matrix(unlist(temp3), nrow=length(temp3), byrow=T))
colnames(old_nicknames) <- parse_row(temp2[1])
rm(temp1, temp2, temp3)

# Save as csv
#write_csv(titles,           paste(in_path, "titles.csv",           sep="/"))
write_csv(male_nicknames,   paste(in_path, "male_nicknames.csv",   sep="/"))
write_csv(female_nicknames, paste(in_path, "female_nicknames.csv", sep="/"))
write_csv(old_nicknames,    paste(in_path, "old_nicknames.csv",    sep="/"))

# Clean up after yourself
rm(list = ls())

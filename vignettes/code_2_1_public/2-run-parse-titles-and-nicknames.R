#########################
# AUTHOR: UGUR YILDIRIM #
# DATE:   2020-05-01    #
#########################

# Source functions
source("/home/ipums/ugury-ipums/censoc-abe-implementation/code/source/parse-titles-and-nicknames.R")

# Load libraries
#library(readr)
#library(tidyverse)

# Set in_path
# in_path must include following four files:
# * titles.txt
# * male_nicknames.txt
# * female_nicknames.txt
# * old_nicknames.txt
in_path <- "/home/ipums/ugury-ipums/censoc-abe-implementation/data/titles-and-nicknames"

# Parse titles and nicknames
parse_titles_and_nicknames(in_path)

# Clean up after yourself
rm(list = ls())

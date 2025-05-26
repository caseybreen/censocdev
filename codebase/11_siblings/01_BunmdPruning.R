library(data.table)
library(tidyverse)


bunmd <- fread("/global/scratch/p2p3/pl1_demography/censoc/workspace/SiblingsCreationClean/bunmd_v2_cleaned.csv")
print(nrow(bunmd))

#---------------Filter data to proper segment---------------
bunmd <- bunmd %>%
  filter(death_age >= 65) %>%
  filter(dyear >= 1988) %>%
  filter(dyear <= 2005) %>%
  filter(death_age <= 110)
print(nrow(bunmd))

#---------------Remove people with incomplete parent names---------------
bunmd <- bunmd %>%
  filter(father_fname != "") %>%
  filter(father_lname != "") %>%
  filter(mother_fname != "") %>%
  filter(mother_lname != "")

bunmd <- bunmd %>%
  filter(father_fname != "unknown") %>%
  filter(father_lname != "unknown") %>%
  filter(mother_fname != "unknown") %>%
  filter(mother_lname != "unknown")

bunmd <- bunmd %>%
  filter(father_fname != "unk") %>%
  filter(father_lname != "unk") %>%
  filter(mother_fname != "unk") %>%
  filter(mother_lname != "unk")

bunmd <- bunmd %>%
  filter(father_fname != "\\") %>%
  filter(father_lname != "\\") %>%
  filter(mother_fname != "\\") %>%
  filter(mother_lname != "\\")

bunmd <- bunmd %>%
  filter(father_fname != "not") %>%
  filter(mother_fname != "not")
print(nrow(bunmd))

bunmd <- bunmd %>%
  filter(nchar(father_fname) > 1) %>%
  filter(nchar(mother_fname) > 1)
print(nrow(bunmd))

#---------------Remove people with same mother and father last name---------------
bunmd <- bunmd %>% filter(father_lname != mother_lname) #something to watch out for
print(nrow(bunmd))

fwrite(bunmd, "/global/scratch/p2p3/pl1_demography/censoc/workspace/SiblingsCreationClean/01_bunmd_pruned.csv")

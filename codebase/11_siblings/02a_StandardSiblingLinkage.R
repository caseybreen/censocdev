library(data.table)
library(tidyverse)

# Read in the pruned file
bunmd_pruned <- fread("/global/scratch/p2p3/pl1_demography/censoc/workspace/SiblingsCreationClean/01_bunmd_pruned.csv")

#Group by parents' first and last names
sibs <- bunmd_pruned %>%
  group_by(father_fname, father_lname, mother_fname, mother_lname) %>%
  mutate(n = n()) %>% # Add variable for group size
  filter(n() > 1) %>%#remove unmatched individuals
  mutate(group_id = cur_group_id()) #Give each group a unique id
nrow(sibs)

#Save ssn to group_id linkage
sibs <- sibs %>% select(starts_with("group_id") | starts_with("ssn"))
fwrite(sibs, "/global/scratch/p2p3/pl1_demography/censoc/workspace/SiblingsCreationClean/02a_StandardSiblings.csv")

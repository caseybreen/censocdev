library(data.table)
library(tidyverse)
library(RecordLinkage)
library(igraph)

#Read in pruned file
bunmd_pruned <- fread("~/SiblingsCreationClean/01_bunmd_pruned.csv")

#Make a dummy variable for the first two characters of parents' names
#This cuts down the grouping to allow the later steps to run quick
bunmd_pruned$m_fname_first <- substr(bunmd_pruned$mother_fname, 1, 2)
bunmd_pruned$m_lname_first <- substr(bunmd_pruned$mother_lname, 1, 2)
bunmd_pruned$f_fname_first <- substr(bunmd_pruned$father_fname, 1, 2)
bunmd_pruned$f_lname_first <- substr(bunmd_pruned$father_lname, 1, 2)

#Group any individuals who match on the variables created above
bunmd_pruned <- bunmd_pruned %>%
  group_by(f_lname_first, f_fname_first, m_lname_first, m_fname_first) %>%
  mutate(n = n())
table(bunmd_pruned$n)

#Remove anyone who has no possible matches after this step
bunmd_pruned <- bunmd_pruned %>%
  filter(n != 1)

#Full join this dataset to create direct pairing for all possible
#matches.
sibs_jaro <- full_join(bunmd_pruned, bunmd_pruned, relationship = "many-to-many",
                       by = c("f_lname_first", "m_fname_first", "m_lname_first", "f_fname_first"))

#Throw out individuals matched with themselves
#Then only retain those whose last names have similarity score of 0.9
sibs_jaro <- sibs_jaro %>% filter(ssn.x != ssn.y)
sibs_jaro <- sibs_jaro %>% filter(jarowinkler(mother_lname.x, mother_lname.y) > 0.9)
sibs_jaro <- sibs_jaro %>% filter(jarowinkler(mother_fname.x, mother_fname.y) > 0.9)
sibs_jaro <- sibs_jaro %>% filter(jarowinkler(father_fname.x, father_fname.y) > 0.9)
sibs_jaro <- sibs_jaro %>% filter(jarowinkler(father_lname.x, father_lname.y) > 0.9)

#Use a graph to find connected segments from the graph of sibling
#linkages.
g <- graph_from_data_frame(sibs_jaro[, c("ssn.x", "ssn.y")])
mem <- components(g, mode="weak")$membership
ssn <- names(mem)
group_id <- mem
jaro_sibs <- data.frame(ssn, group_id) %>%
  group_by(group_id) %>%
  filter(n() > 1)

fwrite(jaro_sibs, "~/SiblingsCreationClean/02b_JaroSiblings_90.csv")

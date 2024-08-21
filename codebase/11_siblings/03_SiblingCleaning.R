library(data.table)
library(tidyverse)

#Load original pruned data and new sibling matchings
bunmd_pruned <- fread("~/SiblingsCreationClean/01_bunmd_pruned.csv")
standard_siblings <- fread("~/SiblingsCreationClean/02a_StandardSiblings.csv")
jaro_siblings <- fread("~/SiblingsCreationClean/02b_JaroSiblings_80.csv")

#Join exact and flex match with bunmd to get all variables back
standard_siblings <- inner_join(bunmd_pruned, standard_siblings, by = "ssn")
jaro_siblings <- inner_join(bunmd_pruned, jaro_siblings, by = "ssn")
nrow(standard_siblings)
nrow(jaro_siblings)

#Remove people with multiple SSN by:
#Removing anyone in siblingship with exact same birth and death day
#Removing anyone in a siblingship with the exact same name
standard_siblings <- standard_siblings %>% 
  distinct(group_id, byear, bmonth, bday, dyear, dmonth, dday, .keep_all = TRUE) %>% 
  distinct(group_id, fname, lname, .keep_all = TRUE) %>%
  group_by(group_id) %>%
  filter(n() > 1)
jaro_siblings <- jaro_siblings %>% 
  distinct(group_id, byear, bmonth, bday, dyear, dmonth, dday, .keep_all = TRUE) %>% 
  distinct(group_id, fname, lname, .keep_all = TRUE) %>%
  group_by(group_id) %>%
  filter(n() > 1)
nrow(standard_siblings)
nrow(jaro_siblings)


#Fix Middle Name Issues

#Add middle initial variable for parents
#Some have longer, but most only have initial letter, so we standardize
standard_siblings <- standard_siblings %>% 
  mutate(f_mname = length(unique(substring(father_mname, 1, 1)))) %>% 
  mutate(m_mname = length(unique(substring(mother_mname, 1, 1))))
jaro_siblings <- jaro_siblings %>% 
  mutate(f_mname = length(unique(substring(father_mname, 1, 1)))) %>% 
  mutate(m_mname = length(unique(substring(mother_mname, 1, 1))))

#Sibship with just 1 middle name per parent
#These don't need cleaning
standard_mname_fine <- standard_siblings %>% filter( !(((f_mname > 2)|((f_mname == 2)&(min(substring(father_mname,1,1)) != ""))) | 
                                                       ((m_mname > 2)|((m_mname == 2)&(min(substring(mother_mname,1,1)) != "")))) )
jaro_mname_fine <- jaro_siblings %>% filter( !(((f_mname > 2)|((f_mname > 1)&(min(substring(father_mname,1,1)) != ""))) | 
                                                 ((m_mname > 2)|((m_mname > 1)&(min(substring(mother_mname,1,1)) != "")))) )

#Sibship with too many mnames per parent
standard_mname_problem <- standard_siblings %>% 
  #Inverse of above, any sib groups with multiple different 
  #parent middle initials
  filter( ((f_mname > 2)|((f_mname == 2)&(min(substring(father_mname,1,1)) != ""))) | 
          ((m_mname > 2)|((m_mname == 2)&(min(substring(mother_mname,1,1)) != ""))) ) %>%
  
  #Remove people with no parents middle names
  #When these people are in groups with multiple parents middle names
  #It is unclear how to group them
  filter(!(substring(father_mname,1,1) == "" & substring(mother_mname,1,1) == "")) %>%
  
  #If one of the parents has a single middle name in these groups
  #Then split only on the other parents name
  filter((f_mname == 1 | (f_mname == 2 & min(substring(father_mname,1,1)) == "")) |
           (m_mname == 1 | (m_mname == 2 & min(substring(mother_mname,1,1)) == ""))) %>%
  mutate(new_father_mname = case_when((f_mname == 2 & min(substring(father_mname,1,1)) == "" ~ max(substring(father_mname,1,1))),
                                      .default = father_mname)) %>%
  mutate(new_mother_mname = case_when((m_mname == 2 & min(substring(mother_mname,1,1)) == "" ~ max(substring(mother_mname,1,1))),
                                      .default = mother_mname)) %>%
  ungroup() %>%
  group_by(group_id, new_father_mname, new_mother_mname) %>%
  filter(n() > 1)

#Repeat of the above for the flexible match
jaro_mname_problem <- jaro_siblings %>% 
  filter( ((f_mname > 2)|((f_mname == 2)&(min(substring(father_mname,1,1)) != ""))) | 
          ((m_mname > 2)|((m_mname == 2)&(min(substring(mother_mname,1,1)) != ""))) ) %>%
  filter(!(substring(father_mname,1,1) == "" & substring(mother_mname,1,1) == "")) %>%
  filter((f_mname == 1 | (f_mname == 2 & min(substring(father_mname,1,1)) == "")) |
           (m_mname == 1 | (m_mname == 2 & min(substring(mother_mname,1,1)) == "")))
jaro_mname_problem <- jaro_mname_problem %>%
  mutate(new_father_mname = case_when((f_mname == 2) ~ max(substring(father_mname,1,1)),
                                      .default = father_mname)) %>%
  mutate(new_mother_mname = case_when((m_mname == 2)  ~ max(substring(mother_mname,1,1)),
                                      .default = mother_mname))
jaro_mname_problem <- jaro_mname_problem %>%
  ungroup() %>%
  group_by(group_id, new_father_mname, new_mother_mname) %>%
  filter(n() > 1)

#Remove large age spread siblings and change group_id to be max ssn
standard_mname_problem <- standard_mname_problem %>%
  mutate(youngest = min(byear)) %>%
  mutate(oldest = max(byear)) %>%
  mutate(age_diff = oldest - youngest) %>%
  filter(age_diff <= 15) %>%
  mutate(group_id = max(ssn)) %>%
  ungroup() %>%
  select(starts_with("ssn") | starts_with("group"))
standard_mname_fine <- standard_mname_fine %>%
  mutate(youngest = min(byear)) %>%
  mutate(oldest = max(byear)) %>%
  mutate(age_diff = oldest - youngest) %>%
  filter(age_diff <= 15) %>%
  mutate(group_id = max(ssn)) %>%
  ungroup() %>%
  select(starts_with("ssn") | starts_with("group"))
jaro_mname_problem <- jaro_mname_problem %>%
  mutate(youngest = min(byear)) %>%
  mutate(oldest = max(byear)) %>%
  mutate(age_diff = oldest - youngest) %>%
  filter(age_diff <= 15) %>%
  mutate(group_id = max(ssn)) %>%
  ungroup() %>%
  select(starts_with("ssn") | starts_with("group"))
jaro_mname_fine <- jaro_mname_fine %>%
  mutate(youngest = min(byear)) %>%
  mutate(oldest = max(byear)) %>%
  mutate(age_diff = oldest - youngest) %>%
  filter(age_diff <= 15) %>%
  mutate(group_id = max(ssn)) %>%
  ungroup() %>%
  select(starts_with("ssn") | starts_with("group"))





#Recombine sibships
standard_siblings <- rbind(standard_mname_fine, standard_mname_problem)
jaro_siblings <- rbind(jaro_mname_fine, jaro_mname_problem)

#Write data
fwrite(standard_siblings, "~/SiblingsCreationClean/03_StandardSiblings.csv")
fwrite(jaro_siblings, "~/SiblingsCreationClean/03_JaroSiblings_80.csv")



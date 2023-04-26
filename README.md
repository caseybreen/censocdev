## censocdev

This repository contains the source code to construct the CenSoc data products. 

The [CenSoc project](https://censoc.berkeley.edu/](https://censoc.berkeley.edu/) creates and makes publicly available Linked Mortality Data Sets, that is, public, individual-level data sets linking the 1940 U.S. Census with the (a) Social Security Death Master File, an (b) the NARA Numident file.

In the `codebase/` directory, each subfolder contains code to: 

- `01_create_bunmd_v2` -- construct the BUNMD 
- `02_create_censoc_v2_1`-- create both the CenSoc-DMF and CenSoc-Numident files  
- `03_create_demo_files_v2_1`-- create the CenSoc-DMF and CenSoc-Numident demo files 
- `04_create_enlistment_records_v2` -- create a cleaned and harmonized version of the military enlistment records and link to the 1940 Census / mortality records [In progress]
- `05_create_censoc_codebooks` -- create the codebooks for all CenSoc datqsets 
- `06_website_vignettes`-- original .Rmd files for all CenSoc website vignettes 
- `07_replication_assess_match_quality` -- replication code for the assess match quality 
- `08_track_censoc_users` -- Count the number of total CenSoc users 

Contributors: 

- Casey Breen
- Maria Osborne 
- Monica Alexander 
- Ugur Yildrim 
- Joshua Goldstein (PI)



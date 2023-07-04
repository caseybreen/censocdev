## CenSoc Development Repository

> A repository for storing all code for creating CenSoc linkages, codebooks, vignettes, and more. 

__Contributors:__ [Casey Breen](https://caseybreen.com),
[Monica Alexander](https://www.monicaalexander.com/),
[Won-tak Joo](https://1takjoo.github.io/),
[Joshua R. Goldstein](https://vcresearch.berkeley.edu/faculty/joshua-goldstein),
[Ugur Yildrim](https://uguryildirim.org/), and
Maria Osborne

---

This repository contains the source code to construct the CenSoc data products. 

The [CenSoc Project](https://censoc.berkeley.edu/) creates and makes publicly available Linked Mortality Data Sets, that is, public, individual-level data sets linking the 1940 U.S. Census with the (a) Social Security Death Master File, an (b) the NARA Numident file.

In the `codebase/` directory, each subfolder contains code to: 

- `01_create_bunmd` — construct the BUNMD 
- `02_create_censoc`— create both the CenSoc-DMF and CenSoc-Numident files  
- `03_create_demo_files`— create the CenSoc-DMF and CenSoc-Numident demo files 
- `04_create_enlistment_records` — create a cleaned and harmonized version of the military enlistment records and link to the 1940 Census / mortality records [in progress]
- `05_create_censoc_codebooks` — create the codebooks for all CenSoc datasets 
- `06_website_vignettes`— all CenSoc website vignettes (.Rmd files and 
- `07_replication_assess_match_quality` — replication code for the assess match quality 
- `08_track_censoc_users` — code to keep track of the number of new and total CenSoc users



## Code to create BUNMD

This subdirectory contains code to create the Berkeley Unified Numident Mortality Database (BUNMD) from the original NARA Numident records. The original NARA numident records were provided as three separate series (deaths, applications, and claims) containing 20 fixed-width text files. The specific codes in this repository: 

- `1_read_numident_fwf.Rmd` - reads in the original fixed-width files from the National Archive for the death records, application records, 
- `2_clean_numident_files.Rmd` - cleans and harmonizes the original NARA Numident records 
- `3_condense_numident_application_and_claims.Rmd` - picks the "best" value for variable when individuals have multiple entires (e.g., 4 application records with alternative spellings of last name) 
- `4_create_bunmd.Rmd` - combines the cleaned, harmonized, and condensed records into one single file (BUNMD). 


Breen, Casey, and Joshua R. Goldstein. 2022. [“Berkeley Unified Numident Mortality Database: Public Administrative Records for Individual-Level Mortality Research.”](https://www.demographic-research.org/volumes/vol47/5/) Demographic Research 47:111–42. doi: 10.4054/DemRes.2022.47.5.



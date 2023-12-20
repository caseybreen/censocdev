## NCHS Weights (in progress)

**Summary:** The scripts in this directory create statistical weights for the CenSoc-Numident and CenSoc-DMF 3.0 datasets. We weight to cells cross-classified by age at death, year of death, sex, race, and birthplace. The full procedure is detailed in our [technical documentation](https://censoc.berkeley.edu/wp-content/uploads/2023/10/CenSoc_V3_Weights_Technical_Report.pdf)

**Important Notes:** NCHS data with birthplace for the year 2005 is restricted and only accessible to researchers who have NCHS approval. These data are stored on a secure computing platform.

`01_merge_and_recode_NCHS_public.R`: Reads and harmonizes public single-year NCHS Multiple Cause of Death data from 1979-2004.

`02_tabulate_NCHS_public.R`: Creates cell counts by age at death, year of death, sex, race, and birthplace for NCHS mortality data 1979-2004.

`03_prepare_numident.R`: Prepares CenSoc-Numident for weighting by creating keys and cell counts with harmonized age at death, year of death, sex, race, and birthplace variables. 

`04_prepare_dmf.R`: Prepares CenSoc-DMF for weighting by creating keys and cell counts with harmonized age at death, year of death, sex, race, and birthplace variables.

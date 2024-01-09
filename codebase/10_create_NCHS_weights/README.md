## NCHS Weights (in progress)

**Summary:** The scripts in this directory create statistical weights for the CenSoc-Numident and CenSoc-DMF 3.0 datasets. We weight to cells cross-classified by age at death, year of death, sex, race, and birthplace. The full procedure is detailed in our [technical documentation](https://censoc.berkeley.edu/wp-content/uploads/2023/10/CenSoc_V3_Weights_Technical_Report.pdf)

**Important Note:** NCHS data with birthplace for the year 2005 is restricted and only accessible to researchers who have NCHS approval. These data are stored on UC Berkeley's Secure Research Data and Compute (SRDC) platform. All CenSoc data imported into the SRDC are completely de-identified and formatted as tables of cell counts so that no re-identification of restricted NCHS data is possible.

The general process of creating weights is as follows:

1. Process public NCHS data and CenSoc datasets to data tables of count for each cross-classified cell in each dataset; import these counts to the SRDC.
2. Inside the SRDC, process restricted NCHS data.
3. Create weights for each cell for each dataset inside the SRDC.
4. Export cell weights and attach these to the full Numident and DMF datasets.


Each script performs the following function:

`01_merge_and_recode_NCHS_public.R`: Reads and harmonizes public single-year NCHS Multiple Cause of Death data from 1979-2004.

`02_tabulate_NCHS_public.R`: Creates cell counts by age at death, year of death, sex, race, and birthplace for NCHS mortality data 1979-2004.

`03_prepare_numident.R`: Prepares CenSoc-Numident for weighting by creating keys and cell counts with harmonized age at death, year of death, sex, race, and birthplace variables. 

`04_prepare_dmf.R`: Prepares CenSoc-DMF for weighting by creating keys and cell counts with harmonized age at death, year of death, sex, race, and birthplace variables.

The tabulated DMF and Numident data can then be transferred to the SRDC. The following scripts use restricted data and can only be run inside the SRDC.

`05_merge_restricted_NCHS_data.R`: Harmonizes and combines single-year restricted NCHS Multiple Cause of Death data from 2005-2020. Note that these are fixed width files; the auxiliary file nchs_fw_layout.csv has been created in order to read the files.

`06_tabulate_restricted_NCHS_data.R`: Creates cell counts by age at death, year of death, sex, race, and birthplace for restricted NCHS mortality data 2005-2020, ages 65+.

`07_create_weights.R`: Creates weights for all ages and years. Calls the function nchs_master_weighting_function.R, which in turn sources a number of functions from nchs_weighting_functions.R. Output is a data table with weights for each cell in each dataset.

No identifiers or identifying information are contained in these data tables, so they can be exported from the SRDC to run the final script:

`08_create_final_weighted_datasets.R`: Reattach weights for each cell back to full CenSoc-DMF and CenSoc-Numident datasets and do some final modifications to create publishable datasets.






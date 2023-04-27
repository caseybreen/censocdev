1-run-split-by-bpl.R

-   Time: ~ 1 hour

-   Goal:

-   Split Census & Numident files by birth state

-   Combine three original Social Security Death Master files into one DMF file.

-   Notes

-   The 1940 Census collected ages at the time of the survey (i.e., 1940 April) and did not ask for exact birth year and date. Therefore, we make a new variable (census_age) using birthdate records in Numident and DMF files (i.e., 1940 minus birth year for those whose birth month is January through March; otherwise, 1939 minus birth year), based on which we match Census data to Numident and DMF files. For DMF, this variable is automatically generated when applying the function load_dmf_deaths.

2-run-parse-titles-and-nicknames.R

-   Time: ~ 1 minute

-   Goal:

-   Make dictionaries for cleaning errors in first names and converting nicknames to formal first names.

3-run-clean-names.R

-   Time: ~12 hours

-   Goal: Clean first names in Census, Numident, and DMF files.

-   Notes

-   This code is to apply the function clean_names to Census, Numident, and DMF files.

-   It takes the longest time among the five application codes since it deals with large string variables.

4-run-match-records.R

-   Time: ~ 6 hours

-   Goal:

-   Perform ABE matching using split and cleaned Census and Numident (or DMF) files.

-   Note

-   This code and the source code match_records are the most complicated ones among the ABE matching application codes.

-   match_records consists of four functions

-   preprocess_A / preprocess_B: keeping necessary variables and generating new variables for matching

-   append_A_to_B: counting rows with the same first & last name and age within each dataset / finding exact matches (e.g., each dataset has only one person with the same first & last name and age) / making variables (e.g., uniquestub_file1, uniquestub_file2) for conservative matches

-   find_nonexact_matches: finding non-exact matches (e.g., no person matched by first & last name with the same age, but some matched within ±1 or ±2 years) / making variables (e.g., uniquestub_match1, uniquestub_match2) for conservative matches

-   fix_ids: storing Census IDs of matched observations to the corresponding Numident (or DMF) data (or vice versa)

-   match_records matches two datasets twice forward (i.e., Census to Numident/DMF) and backward (i.e., Numident/DMF to Census), and takes only those identified as matched observations in both directions

-   When matching Census to Numident, (i) males, (ii) ever-married females, and (iii) never-married females (using father's last name in Numident) are matched in turn.

-   Conservative matches are defined by two variables, uniquestub_fileN (i.e., first & last names are unique within ±N age within the dataset) and uniquestub_matchN (i.e., matched to one person within ±N age in the corresponding dataset). For example, you can identify conservative matches within ±N years by taking those with uniquestub_file2==1 and unique_stubmatch==1.

5-generate-final-v2-1-matched-datasets.R

-   Time: ~ 5 minutes

-   Goal

-   Rename and filter variables.

-   Produce final matched datasets.

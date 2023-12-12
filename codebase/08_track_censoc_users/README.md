## Track CenSoc Users 

The code in these scripts logs people who have signed up to use or download CenSoc data. **As of May 31st, 2023, CenSoc data are now downloadable through Harvard Dataverse rather than our bespoke website.** 

`track_custom_censoc_download_page.Rmd`: The code in this notebook generates reports and figures about the total number of new CenSoc users who signed up using our bespoke data download page. Specifically, this code accesses the log of all people who have signed up to download CenSoc data. We report the number of unique people who have ever signed up to download Censoc data as our number of users. The code also links each user to the specific data files they downloaded using the Apache web logs. **While this webpage has not been decomissioned, we no longer update it or link to it. Most downloads after May 31st, 2023 will be through Harvard Dataverse.**

`track_dataverse_users.R`: The code in this script processes the Harvard Dataverse guestbook, a log of user info from each download of CenSoc data products from Dataverse. Unlike the bespoke data download page, Harvard Dataverse prompts users to enter their name/email each time they download a dataset or codebook instead of signing up once to access all data products. **Most downloads after May 31st, 2023 will be through Harvard Dataverse.** However, during the period of approximately July 2023 through September 2023, user info for downloaders of the CenSoc-Numident and CenSoc-DMF datsets was not logged.


---
title: "Exploring Mortality Differentials in the CenSoc-DMF Demo Dataset"
author: "Casey Breen (caseybreen@berkeley.edu)"
date: "2020-05-03"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Vignette Title}
  %\VignetteEngine{knitr::rmarkdown}
  \usepackage[utf8]{inputenc}
---

**Summary**: This vignette gives an overview of the CenSoc-DMF Demo dataset and presents two stylized examples on state variation in longevity and wage income. The goal of this vignette is to give users a high-level overview of working with CenSoc data, including the use of weights, the specification and visualization of regression models, and the use of the `WAGEINC` variable in the 1940 census. 

The CenSoc-DMF Demo dataset was constructed by (i) linking the CenSoc-DMF dataset to the IPUMS 1940 1% census sample and (ii) selecting a set of 20 mortality covariates from the 1940 census. The smaller size of the file — approximately 1% of the records in the full CenSoc-DMF dataset — makes it easier to work with but precludes the high-resolution mortality research possible with the full CenSoc-DMF dataset. 

Before getting started with the vignette, make sure to: 

- [Download](https://censoc-download.demog.berkeley.edu/) the CenSoc DMF Demo File 
- Install packages if necessary (use the `install.packages()` function)
    - `tidyverse`
    - `statebins`
    - `brooms`
    - `knitr`
    
The original R notebook (.Rmd file) for this vignette can be downloaded [here](https://github.com/caseybreen/censocdev/tree/master/vignettes/website). 








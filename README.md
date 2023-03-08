Code files corresponding to the 'Association of Race and Ethnicity with Priority for Deceased Donor Kidney Transplant'. <br />

All files are written and tested using R 4.2.2. Files should be run in this order:

### 1. candidates.R
Sets up dataset of all 2015-2020 kidney waitlist candidates from over 18 and excluding living donors from the SRTR (Scientific Registry of Transplant Recipients) CAND_KIPA raw data. Analysis is done at the registration level.

EPTS Percentiles are based on the 2020 Mapping Table, available at: https://optn.transplant.hrsa.gov/media/wn3buk04/epts_mapping_table_2020.pdf.

Updated 2/16/23: Different dialysis time variables are created. Additional gender variable is added.


### 2. recipients.R
Sets up corresponding dataset of kidney transplant recipients, based on SRTR TX_KI data, merged with the previous candidates dataset.

Updated 2/16/23: Different dialysis time variables are created. Additional gender variable is added.


### 3. candidates_analysis.Rmd
Runs tabulations and multivariable regressions for EPTS and KDPI scores in the candidates dataset. Runs competing risks analysis with KDPI score.

Updated 11/15/22: Figures are changed.

Updated 11/29/22: Incorrect labelling is changed.

Updated 2/22/23: Changed to reflect updated variables. Labelling is updated. Error bars added.


### 4. recipients_analysis.Rmd
Runs survival analysis for EPTS scores in the recipients dataset. Primarily sensitivity analyses and supplementary material.

Updated 3/8/23: Updated to include all post-transplant recipient analyses.

### 5. recipients_analysis_v2.Rmd (removed)
Added 2/16/23: Additional sensitivity competing-risks and outcomes analyses in the recipients dataset. Can be taken as supplementary material.

Updated 3/3/23: Changed competing risks error.

Updated 3/8/23: Removed.

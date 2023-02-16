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

Updated 2/16/23: Changed to reflect updated variables. Labelling is updated.


### 4. recipients_analysis.Rmd
Runs survival analysis for EPTS scores in the recipients dataset. 

Updated 2/16/23: Changed to reflect updated variables. Labelling is updated.

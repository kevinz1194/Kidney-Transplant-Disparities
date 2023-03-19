Code files corresponding to the 'Association of Race and Ethnicity with Priority for Deceased Donor Kidney Transplant'. <br />

All files are written and tested using R 4.2.2. Files should be run in this order:

### 1. candidates.R
Sets up dataset of all 2015-2020 kidney waitlist candidates from over 18 and excluding living donors from the SRTR (Scientific Registry of Transplant Recipients) CAND_KIPA raw data. Analysis is done at the registration level.

EPTS Percentiles are based on the 2020 Mapping Table, available at: https://optn.transplant.hrsa.gov/media/wn3buk04/epts_mapping_table_2020.pdf.

Updated 2/16/23: Different dialysis time variables are created. Additional gender variable is added.

Updated 3/8/23: Code is overhauled to add alternative dataset with patients waitlisted in 2015 removed, and to work with the other files.



### 2. candidates_livingdonor.R 
Sets up an alternative version of the candidates dataset. Includes those transplanted with a living donor.

Updated 3/8/23: Added.


### 3. recipients.R
Sets up corresponding dataset of kidney transplant recipients, based on SRTR TX_KI data.

Updated 2/16/23: Different dialysis time variables are created. Additional gender variable is added.

Updated 3/8/23: Code is overhauled, time variables are reworked. Merging with candidates file is now performed in
recipients_analysis.Rmd.



### 4. candidates_analysis.Rmd
Runs tabulations and multivariable regressions for EPTS and KDPI scores in the candidates dataset. Runs competing risks analysis with KDPI score.

Updated 11/15/22: Figures are changed.

Updated 11/29/22: Incorrect labelling is changed.

Updated 2/22/23: Changed to reflect updated variables. Labelling is updated. Error bars added.

Updated 3/8/23: Changed to work with the new output files.

Updated 3/17/23: Sections removed to contain only necessary code for article. Updated to include living donors.


### 5. candidates_analysis_sensitivity.Rmd 
Candidate analyses on the two alternative candidate datasets.

Updated 3/8/23: Added.

Updated 3/17/23: Sections removed to contain only necessary code for article. Updated to include living donors.


### 6. recipients_analysis.Rmd
Runs survival analysis for EPTS scores in the recipients dataset. Requires files produced by candidates.R and recipients.R.

Updated 3/8/23: Updated to include all post-transplant recipient and competing-risks analyses.

Updated 3/15/23: Sections removed to contain only necessary code for article.

Updated 3/19/23: Added sensitivity analysis with top 20% KDPI filter.


### 7. recipients_analysis_v2.Rmd 
Added 2/16/23: Additional sensitivity competing-risks and outcomes analyses in the recipients dataset. Can be taken as supplementary material.

Updated 3/3/23: Changed competing risks error.

Updated 3/8/23: Removed.

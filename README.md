Code files corresponding to the 'Association of Race and Ethnicity with Priority for Deceased Donor Kidney Transplant'. <br />

All files are written and tested using R 4.2.2. Files should be run in this order:


### 1. KDPI_for_EPTS.Rmd
Sets up dataset to calculate KDPI score, based on the April 2022 guide available at: https://optn.transplant.hrsa.gov/media/j34dm4mv/kdpi_guide.pdf.

KDRI to KDPI is based on the March 2022 Mapping Table, available at: https://optn.transplant.hrsa.gov/media/wnmnxxzu/kdpi_mapping_table.pdf.

Updated 5/10/23: Added.


### 2. candidate_data_prep.Rmd
Sets up dataset of all 2015-2020 kidney waitlist candidates from over 18 and excluding living donors from the SRTR (Scientific Registry of Transplant Recipients) CAND_KIPA raw data. Analysis is done at the registration level.

EPTS score is based on the April 2022 guide available at: https://optn.transplant.hrsa.gov/media/pn1pt2bc/epts_guide.pdf. 

EPTS percentiles are based on the March 2022 Mapping Table, available at: https://optn.transplant.hrsa.gov/media/g3xfajp4/epts_mapping_table.pdf.

Updated 2/16/23: Different dialysis time variables are created. Additional gender variable is added.

Updated 3/8/23: Code is overhauled to add alternative dataset with patients waitlisted in 2015 removed, and to work with the other files.

Updated 5/12/23: Renamed to candidate_data_prep and converted to a notebook. Includes living donors.


### 3. recipients_data_prep.Rmd
Sets up corresponding dataset of kidney transplant recipients, based on SRTR TX_KI data.

Updated 2/16/23: Different dialysis time variables are created. Additional gender variable is added.

Updated 3/8/23: Code is overhauled, time variables are reworked. Merging with candidates file is now performed in
recipients_analysis.Rmd.

Updated 5/12/23: Renamed to recipients_data_prep and converted to a notebook. Selection criteria is now based on transplant date, not listing date.

Updated 5/15/23: Race variable modified to become more accurate.


### 4. candidates_analysis.Rmd
Runs tabulations and multivariable regressions for EPTS and KDPI scores in the candidates dataset. Runs competing risks analysis with KDPI score.

Updated 11/15/22: Figures are changed.

Updated 11/29/22: Incorrect labelling is changed.

Updated 2/22/23: Changed to reflect updated variables. Labelling is updated. Error bars added.

Updated 3/8/23: Changed to work with the new output files.

Updated 3/17/23: Sections removed to contain only necessary code for article. Updated to include living donors.

Updated 3/22/23: Tables updated.

Updated 5/12/23: Updated to be compatible with the new code.


### 5. candidates_analysis_sensitivity.Rmd 
Candidate analyses on the two alternative candidate datasets.

Updated 3/8/23: Added.

Updated 3/17/23: Sections removed to contain only necessary code for article. Updated to include living donors.

Updated 3/22/23: Tables updated.

Updated 5/12/23: Updated to be compatible with the new code.


### 6. recipients_analysis.Rmd
Runs survival analysis for EPTS scores in the recipients dataset. Requires files produced by candidates.R and recipients.R.

Updated 3/8/23: Updated to include all post-transplant recipient and competing-risks analyses.

Updated 3/15/23: Sections removed to contain only necessary code for article.

Updated 3/19/23: Added sensitivity analysis with top 20% KDPI filter.

Updated 5/12/23: Updated to be compatible with the new code.

################################################
# Code for work with recipient data
#
# 1. Set up TX_KI dataset and clean (Line 23)
# 2. Merge with candidate dataset (Line 110)
# 3. Calculate EPTS score and finalize (Line 187)
# 4. Create new post-transplant variables (Line 315)
#
# Last Modified: K. Zhang (2/16/23)
#
################################################
rm(list=ls()); gc()
if (!require('pacman')) {install.packages('pacman')}
library(pacman)
pacman::p_load(tidyverse, Hmisc, lubridate, haven, expss)



################################################
# 1. Set up TX_KI dataset and clean
################################################
### Read in TX_KI dataset and select relevant variables
## Limit data to 2015-2020 and exclude live donor recipients, age 18 or older
df_tx_ki <- haven::read_sas('./tx_ki.sas7bdat')
df_tx_ki <- df_tx_ki %>%
  filter(as.Date(REC_TX_DT) >= '2015-01-01' & as.Date(REC_TX_DT) <= '2020-12-31') %>%
  filter(ORG_TY== 'KI') %>%
  filter(CAN_AGE_AT_LISTING >= 18) %>%
  
  select(PX_ID, PERS_ID, CAN_RACE, CAN_GENDER, CAN_LISTING_DT,
         CAN_PREV_TX, PERS_OPTN_DEATH_DT, PERS_SSA_DEATH_DT, CAN_DIAB_TY,
         TFL_DEATH_DT, TFL_LAFUDATE, REC_PRETX_DIAL, REC_DIAL_DT, REC_AGE_AT_TX, REC_TX_DT,
         TFL_GRAFT_DT, DON_TY)


df_tx_ki <- expss::drop_all_labels(df_tx_ki) 


######### ALTERNATIVE METHOD: DROP DUPLICATE VALUES ####################
### For duplicates in PERS_ID, use first PX_ID observation
# df_tx_ki <- df_tx_ki[order(df_tx_ki$PERS_ID, df_tx_ki$PX_ID),]
# df_tx_ki <- df_tx_ki %>% 
#  group_by(PERS_ID) %>%
#  filter(row_number(PERS_ID) == 1)
#########################################################################


### Clean up variables
df_tx_ki <- df_tx_ki %>%
  mutate(
    living_donor = case_when(
      DON_TY == 'L' ~ 1,
      TRUE ~ 0),
    
    previous_TX_KI = factor(CAN_PREV_TX, levels=c(0,1)),
    
    dialysis_dt = as.Date(REC_DIAL_DT),
    
    preemptive_listing_KI = case_when(
      REC_PRETX_DIAL == 'N' ~ '1',
      TRUE ~ '0'),
    
    sex_tx = case_when(
      CAN_GENDER == 'M' ~ 'Male',
      CAN_GENDER == 'F' ~ 'Female'),
    
    age_tx = as.numeric(REC_AGE_AT_TX),
    
    death_date_tx = as.Date(TFL_DEATH_DT),
    
    transplant_date_tx = as.Date(REC_TX_DT),
    
    graft_failure_dt = as.Date(TFL_GRAFT_DT),
    
    followup_dt = as.Date(TFL_LAFUDATE),
    
    listing_dt_tx = as.Date(CAN_LISTING_DT),
    
    race_tx = case_when(
      CAN_RACE == '8' ~ 'White',
      CAN_RACE == '16' ~ 'Black_AA',
      CAN_RACE == '32' ~ 'American_Indian',
      CAN_RACE == '64' ~ 'Asian',
      CAN_RACE == '128' ~ 'Pacific_Islander',
      CAN_RACE == '256' ~ 'Arab_Middle_East',
      CAN_RACE == '512' ~ 'Indian_SubContinent',
      CAN_RACE == '1024' ~ 'Unknown',
      CAN_RACE == '2000' ~ 'Hispanic',
      TRUE ~ 'Other'),
    
    diabetes_cat_tx = case_when(
      CAN_DIAB_TY == '2' | CAN_DIAB_TY == '3' | CAN_DIAB_TY == '4' | CAN_DIAB_TY == '5' ~ '1',
      TRUE ~ '0')) %>%
  
  select(-c(CAN_GENDER, CAN_RACE, CAN_LISTING_DT, CAN_DIAB_TY, CAN_PREV_TX, REC_PRETX_DIAL, 
            REC_AGE_AT_TX, REC_DIAL_DT, REC_TX_DT,
            PERS_OPTN_DEATH_DT, PERS_SSA_DEATH_DT, TFL_DEATH_DT, TFL_LAFUDATE, TFL_GRAFT_DT, DON_TY))
df_tx_ki$race_tx <- factor(df_tx_ki$race_tx, levels=c('White', 'American_Indian', 'Arab_Middle_East',
                                                      'Asian', 'Black_AA', 'Hispanic', 'Indian_SubContinent',
                                                      'Other', 'Pacific_Islander', 'Unknown'))



################################################
# 2. Merge with candidate dataset and finalize
################################################
load('./candidates_v2.RData')

df_recipients <- merge(df_tx_ki, df_cand_kipa, by = 'PX_ID', all.x = T, all.y = F)
rm(df_cand_kipa, df_tx_ki)


df_recipients <- df_recipients %>%
  select(-c(PERS_ID.y, listing_date, time, age, age_group, sex, raw_epts, percentile_epts, 
            dialysis_time_at_transplant, dialysis_time_at_list,
            preemptive_transplant, preemptive_listing,
            transplant, transplant_date,
            removal_code, removal_date, death_after_TX, race, diabetes,
            death_date)) %>%
  
  mutate(age_tx = as.numeric(age_tx)) %>%
  
  mutate(age_group = case_when(
    age_tx <= 24 ~ '18-24',
    age_tx > 24 & age_tx <= 29 ~ '25-29',
    age_tx > 29 & age_tx <= 39 ~ '30-39',
    age_tx > 39 & age_tx <= 49 ~ '40-49',
    age_tx > 49 & age_tx <= 59 ~ '50-59',
    age_tx > 59 & age_tx <= 69 ~ '60-69',
    TRUE ~ '>70'))


######################################################
### Confirm previous variables are consistent
# table(df_recipients$previous_TX, df_recipients$previous_TX_KI)
# df_recipients$previous_TX <- NULL
# 
# table(df_recipients$death, df_recipients$death_tx)
# df_recipients$death <- NULL
######################################################

### Transplant time
df_recipients$transplant_time <- as.numeric(difftime(df_recipients$transplant_date_tx, 
                                                     df_recipients$listing_dt_tx, units = 'weeks')) / 52.25


### Dialysis years prior to transplant
df_recipients <- df_recipients%>% 
  mutate(
    dialysis_time_at_transplant = case_when(
      is.na(dialysis_dt) | is.na(transplant_date_tx) ~ 0,
      TRUE ~ as.numeric(difftime(dialysis_dt, transplant_date_tx, units = 'weeks')) / 52.25))
df_recipients$dialysis_time_at_transplant <- -round(df_recipients$dialysis_time_at_transplant, digits = 1)


### Time to death with functioning graft
df_recipients <- df_recipients %>% 
  mutate(time_to_death_function = 
           case_when(!is.na(death_date_tx) == '1' ~ 
                       as.numeric(as.Date(death_date_tx) - as.Date(transplant_date_tx), units = 'days')))

### Time to graft failure
df_recipients$time_graft_failure <- 
  as.numeric(df_recipients$graft_failure_dt - df_recipients$transplant_date_tx, units = 'days')


### Graft survival time 
df_recipients <- df_recipients %>% 
  mutate(time_graft_survival = 
           case_when(
             !is.na(death_date_tx) | !is.na(graft_failure_dt) ~
               as.numeric(as.Date(pmin(death_date_tx, graft_failure_dt, na.rm = T)) - transplant_date_tx, 
                          units = 'days')))


### Time to followup
df_recipients$time_fu <- as.numeric(df_recipients$followup_dt - df_recipients$transplant_date_tx, units = 'days')



################################################
# 3. Calculate EPTS score and finalize
################################################
### Raw EPTS scores
df_recipients <- df_recipients %>% 
  mutate(raw_epts = 
           0.047*pmax(age_tx - 25, 0) -
           0.015*(as.numeric(diabetes_cat_tx))*pmax(age_tx - 25, 0) +
           0.398*(as.numeric(previous_TX_KI)) - 0.237*(as.numeric(diabetes_cat_tx))*((as.numeric(previous_TX_KI))) +
           0.315*log(dialysis_time_at_transplant + 1)- 0.099*(diabetes_cat_tx=='1')*log(dialysis_time_at_transplant + 1) +
           0.130*(dialysis_time_at_transplant==0) - 0.348*(dialysis_time_at_transplant==0) +  
           1.262*(as.numeric(diabetes_cat_tx))
  )


### Percentile EPTS scores
df_recipients <- df_recipients %>%
  mutate(percentile_epts = case_when(
    raw_epts <= 0.03659817399191 ~ 0,
    raw_epts <= 0.25452908966461 ~ 1,
    raw_epts <= 0.42352944598300 ~ 2,
    raw_epts <= 0.53003080082136 ~ 3,
    raw_epts <= 0.62358042436687 ~ 4,
    raw_epts <= 0.71575920635007 ~ 5,
    raw_epts <= 0.79651301542106 ~ 6,
    raw_epts <= 0.86920864832432 ~ 7,
    raw_epts <= 0.93691375770021 ~ 8,
    raw_epts <= 1.00115259133800 ~ 9,
    raw_epts <= 1.06147501711157 ~ 10,
    raw_epts <= 1.11886399323084 ~ 11,
    raw_epts <= 1.17274132750756 ~ 12,
    raw_epts <= 1.22455411031063 ~ 13,
    raw_epts <= 1.27405270362765 ~ 14,
    raw_epts <= 1.32239366251146 ~ 15,
    raw_epts <= 1.36840427006353 ~ 16,
    raw_epts <= 1.41186789869952 ~ 17,
    raw_epts <= 1.45301619533377 ~ 18,
    raw_epts <= 1.49435112936345 ~ 19,
    raw_epts <= 1.53286770421574 ~ 20,
    raw_epts <= 1.57155441478439 ~ 21,
    raw_epts <= 1.60668450477357 ~ 22,
    raw_epts <= 1.64078781656400 ~ 23,
    raw_epts <= 1.67138347106792 ~ 24,
    raw_epts <= 1.70048254984577 ~ 25,
    raw_epts <= 1.72867145790554 ~ 26,
    raw_epts <= 1.75678302532512 ~ 27,
    raw_epts <= 1.78276238577397 ~ 28,
    raw_epts <= 1.80864887063655 ~ 29,
    raw_epts <= 1.83411430527036 ~ 30,
    raw_epts <= 1.85819479732733 ~ 31,
    raw_epts <= 1.88139546861610 ~ 32,
    raw_epts <= 1.90423427159151 ~ 33,
    raw_epts <= 1.92684448967469 ~ 34,
    raw_epts <= 1.95017274194208 ~ 35,
    raw_epts <= 1.97209769030884 ~ 36,
    raw_epts <= 1.99285503839760 ~ 37,
    raw_epts <= 2.01466894405460 ~ 38,
    raw_epts <= 2.03390212183436 ~ 39,
    raw_epts <= 2.05369746748802 ~ 40,
    raw_epts <= 2.07340246406571 ~ 41,
    raw_epts <= 2.09208772554548 ~ 42,
    raw_epts <= 2.11008008213552 ~ 43,
    raw_epts <= 2.12825462012320 ~ 44,
    raw_epts <= 2.14538566530003 ~ 45,
    raw_epts <= 2.16347433264887 ~ 46,
    raw_epts <= 2.17963031413852 ~ 47,
    raw_epts <= 2.19604187305641 ~ 48,
    raw_epts <= 2.21302327173169 ~ 49,
    raw_epts <= 2.22890885657669 ~ 50,
    raw_epts <= 2.24521560574949 ~ 51,
    raw_epts <= 2.26099514620549 ~ 52,
    raw_epts <= 2.27693714028400 ~ 53,
    raw_epts <= 2.29328883840429 ~ 54,
    raw_epts <= 2.30873374401095 ~ 55,
    raw_epts <= 2.32508761122519 ~ 56,
    raw_epts <= 2.34046399709776 ~ 57,
    raw_epts <= 2.35469883641342 ~ 58,
    raw_epts <= 2.36976221190132 ~ 59,
    raw_epts <= 2.38548117727584 ~ 60,
    raw_epts <= 2.40080210992761 ~ 61,
    raw_epts <= 2.41601323013786 ~ 62,
    raw_epts <= 2.43071022841615 ~ 63,
    raw_epts <= 2.44635936535286 ~ 64,
    raw_epts <= 2.46081562059785 ~ 65,
    raw_epts <= 2.47507281008807 ~ 66,
    raw_epts <= 2.49097050616980 ~ 67,
    raw_epts <= 2.50655989048597 ~ 68,
    raw_epts <= 2.52244591190598 ~ 69,
    raw_epts <= 2.53838298001874 ~ 70,
    raw_epts <= 2.55365614791238 ~ 71,
    raw_epts <= 2.56946475017112 ~ 72,
    raw_epts <= 2.58636928647468 ~ 73,
    raw_epts <= 2.60244038836947 ~ 74,
    raw_epts <= 2.61972240525357 ~ 75,
    raw_epts <= 2.63526078028747 ~ 76,
    raw_epts <= 2.65245419737158 ~ 77,
    raw_epts <= 2.67072065357604 ~ 78,
    raw_epts <= 2.68757487707999 ~ 79,
    raw_epts <= 2.70595570009828 ~ 80,
    raw_epts <= 2.72401095140315 ~ 81,
    raw_epts <= 2.74286793947642 ~ 82,
    raw_epts <= 2.76238315880849 ~ 83,
    raw_epts <= 2.78086498100229 ~ 84,
    raw_epts <= 2.80019769964201 ~ 85,
    raw_epts <= 2.82065812497000 ~ 86,
    raw_epts <= 2.84154403217207 ~ 87,
    raw_epts <= 2.86302665309162 ~ 88,
    raw_epts <= 2.88448137367205 ~ 89,
    raw_epts <= 2.90710010789073 ~ 90,
    raw_epts <= 2.93088554597975 ~ 91,
    raw_epts <= 2.95633172813984 ~ 92,
    raw_epts <= 2.98243616034011 ~ 93,
    raw_epts <= 3.01100193655189 ~ 94,
    raw_epts <= 3.04363481752507 ~ 95,
    raw_epts <= 3.07945530651501 ~ 96,
    raw_epts <= 3.11944421677048 ~ 97,
    raw_epts <= 3.17254475204181 ~ 98,
    TRUE ~ 100)) %>%
      
      mutate(top_percentile_epts = case_when(
        percentile_epts <= 20 ~ '1', 
        TRUE ~ '0')) 

    
df_recipients$top_percentile_epts = factor(df_recipients$top_percentile_epts, levels=c(0,1))
  


################################################
# 4. Create new post-transplant variables
################################################
### Death with functioning graft (kidney)
df_recipients <- df_recipients %>%
  mutate(death_with_function = 
           case_when(!is.na(death_date_tx) & is.na(graft_failure_dt) ~ 1,
                     TRUE ~ 0),
         
         graft_failure =
           case_when(!is.na(graft_failure_dt) ~ 1,
                     TRUE ~ 0),
         
         graft_survival = 
           case_when(
             !is.na(graft_failure_dt) | !is.na(death_date_tx) ~ 1,
             TRUE ~ 0))



df_recipients <- df_recipients[ ,c('PX_ID', 'PERS_ID.x', 'raw_epts', 'percentile_epts', 'top_percentile_epts',
                                   'time_to_death_function', 'time_graft_survival', 'time_graft_failure',
                                   'time_fu','transplant_time', 'graft_failure_dt',
                                   'death_with_function', 'graft_failure', 'graft_survival', 
                                   'listing_dt_tx', 'death_date_tx',
                                   'previous_TX_KI', 'dialysis_dt', 'preemptive_listing_KI', 
                                   'transplant_date_tx', 
                                   'age_tx', 'age_group', 'sex_tx', 'race_tx', 'diabetes_cat_tx',  
                                   'dialysis_time_at_transplant', 'living_donor', 'kdpi', 'top20_kdpi')]


colnames(df_recipients) <- c('PX_ID', 'PERS_ID', 'raw_epts', 'percentile_epts', 'top_percentile_epts', 
                             'time_to_death_function', 'time_graft_survival', 'time_graft_failure', 
                             'time_fu', 'transplant_time', 'graft_failure_dt',
                             'death_with_function', 'graft_failure', 'graft_survival', 
                             'listing_dt', 'death_dt', 'previous_TX', 'dialysis_dt', 'preemptive_listing', 
                             'transplant_dt', 'age_tx', 'age_group', 'sex_tx', 'race', 'diabetes_cat', 
                             'dialysis_time_at_transplant', 'living_donor', 'kdpi', 'top20_kdpi')




save(df_recipients, file = './recipients_v2.RData')
write.csv(df_recipients, file = './recipients_v2.csv')

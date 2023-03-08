################################################
# Code for work with recipient data
#
# 1. Set up TX_KI dataset and clean (Line 21)
# 2. Create time variables and finalize (Line 99)
#
# Last Modified: K. Zhang (3/7/23)
#
################################################
rm(list=ls()); gc()
setwd('C:/Users/kevinz94/Desktop/EPTS_Score')
if (!require('pacman')) {install.packages('pacman')}
library(pacman)
pacman::p_load(tidyverse, Hmisc, lubridate, haven, expss)



################################################
# 1. Set up TX_KI dataset and clean
################################################
### Read in TX_KI dataset and select relevant variables
## Limit data to 2015-2020 and exclude live donor recipients, age 18 or older
df_recipients <- haven::read_sas('./tx_ki.sas7bdat')
df_recipients <- df_recipients %>%
  filter(as.Date(CAN_LISTING_DT) >= '2015-01-01' & as.Date(CAN_LISTING_DT) <= '2020-12-31') %>%
  filter(ORG_TY== 'KI') %>%
  filter(CAN_AGE_AT_LISTING >= 18) %>%
  
  select(PX_ID, PERS_ID, CAN_RACE, CAN_GENDER, CAN_LISTING_DT,
         CAN_PREV_TX, PERS_OPTN_DEATH_DT, PERS_SSA_DEATH_DT, CAN_DIAB_TY,
         TFL_DEATH_DT, TFL_LAFUDATE, REC_PRETX_DIAL, REC_DIAL_DT, REC_AGE_AT_TX, REC_TX_DT,
         TFL_GRAFT_DT, DON_TY) %>%
  
  filter(CAN_DIAB_TY != '998')


df_recipients <- expss::drop_all_labels(df_recipients) 


### Clean up variables
df_recipients <- df_recipients %>%
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


df_recipients$race_tx <- factor(df_recipients$race_tx, levels=c('White', 'American_Indian', 'Arab_Middle_East',
                                                      'Asian', 'Black_AA', 'Hispanic', 'Indian_SubContinent',
                                                      'Other', 'Pacific_Islander', 'Unknown'))



################################################
# 2. Create time variables and finalize
################################################
### Dialysis years prior to transplant
df_recipients <- df_recipients %>% 
  mutate(
    dialysis_time_at_transplant = case_when(
      is.na(dialysis_dt) | is.na(transplant_date_tx) ~ 0,
      TRUE ~ as.numeric(difftime(dialysis_dt, transplant_date_tx, units = 'weeks')) / 52.25))
df_recipients$dialysis_time_at_transplant <- -round(df_recipients$dialysis_time_at_transplant, digits = 1)


#############
### Time from transplant to followup
df_recipients$time_tx_fu_days <- as.numeric(as.Date(df_recipients$followup_dt) - 
                                              as.Date(df_recipients$transplant_date_tx), units = 'days')


#############
### Time from transplant to death (death with functioning graft)
df_recipients <- df_recipients %>%
  
  mutate(death_functioning_graft = case_when(
    (!is.na(death_date_tx) & (as.Date(death_date_tx) >= as.Date(transplant_date_tx))) ~ 1,
    TRUE ~ 0))

df_recipients$time_tx_death_days <- NA

df_recipients$time_tx_death_days[df_recipients$death_functioning_graft == '1'] <-
  as.numeric(as.Date(df_recipients$death_date_tx[df_recipients$death_functioning_graft == '1']) - 
               as.Date(df_recipients$transplant_date_tx[df_recipients$death_functioning_graft == '1']), units = 'days') 

df_recipients$time_tx_death_days[is.na(df_recipients$time_tx_death_days)] <- 
  df_recipients$time_tx_fu_days[is.na(df_recipients$time_tx_death_days)]


#############
### Time from transplant to graft failure
df_recipients <- df_recipients %>%
  
  mutate(graft_failure = case_when(
    !is.na(graft_failure_dt) ~ 1,
    TRUE ~ 0))

df_recipients$time_tx_graft_failure_days <- NA

df_recipients$time_tx_graft_failure_days[df_recipients$graft_failure == '1'] <-
  as.numeric(as.Date(df_recipients$graft_failure_dt[df_recipients$graft_failure == '1']) - 
               as.Date(df_recipients$transplant_date_tx[df_recipients$graft_failure == '1']), units = 'days') 

df_recipients$time_tx_graft_failure_days[is.na(df_recipients$time_tx_graft_failure_days)] <- 
  df_recipients$time_tx_fu_days[is.na(df_recipients$time_tx_graft_failure_days)]


#############
### Graft survival time from transplant
df_recipients <- df_recipients %>%
  
  mutate(graft_survival = case_when(
    !is.na(df_recipients$death_date_tx) | !is.na(df_recipients$graft_failure_dt) ~ 1,
    TRUE ~ 0))


df_recipients$time_tx_graft_survival_days <- NA
df_recipients$time_tx_graft_survival_days[!is.na(df_recipients$death_date_tx) | !is.na(df_recipients$graft_failure_dt)] <-
  as.numeric(as.Date(pmin(df_recipients$death_date_tx[!is.na(df_recipients$death_date_tx) | 
                                                        !is.na(df_recipients$graft_failure_dt)], 
                          df_recipients$graft_failure_dt[!is.na(df_recipients$death_date_tx) | 
                                                           !is.na(df_recipients$graft_failure_dt)], na.rm = T)) -
               as.Date(df_recipients$transplant_date_tx[!is.na(df_recipients$death_date_tx) | 
                                                          !is.na(df_recipients$graft_failure_dt)]),  units = 'days') 


df_recipients$time_tx_graft_survival_days[is.na(df_recipients$time_tx_graft_survival_days)] <- 
  df_recipients$time_tx_fu_days[is.na(df_recipients$time_tx_graft_survival_days)]






df_recipients <- df_recipients[ ,c('PX_ID', 'PERS_ID', 'living_donor', 'previous_TX_KI', 'dialysis_dt', 
                                   'preemptive_listing_KI', 'sex_tx', 'race_tx', 'age_tx', 'death_date_tx', 
                                   'transplant_date_tx', 'graft_failure_dt', 'followup_dt', 
                                   'diabetes_cat_tx', 'dialysis_time_at_transplant', 'death_functioning_graft', 
                                   'time_tx_death_days', 'graft_failure', 'time_tx_graft_failure_days', 
                                   'graft_survival', 'time_tx_graft_survival_days', 'time_tx_fu_days')]



save(df_recipients, file = './recipients.RData')
write.csv(df_recipients, file = './recipients.csv')

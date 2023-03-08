################################################
# Code for work with candidate data, alternative datasets
#
# 1. Set up candidate dataset (Line 23)
# 2. Add EPTS scores (Line 86)
# 3. Clean variables (Line 215)
# 4. Add kipa scores and finalize candidate dataset (Line 361)
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
# 1. Set up candidate dataset
################################################
df_cand_kipa <- haven::read_sas('./cand_kipa.sas7bdat')

## Limit data to 2015-2020 (live donors are included)
### Added: filter to those age 18 or older at listing ###
df_cand_kipa <- df_cand_kipa %>%
  filter(as.Date(CAN_LISTING_DT) >= '2015-01-01' & as.Date(CAN_LISTING_DT) <= '2020-12-31') %>%
  filter(WL_ORG == 'KI') %>%
  filter(CAN_AGE_AT_LISTING >= 18)


## Drop observations with missing/unknown diabetes, age, and previous transplant
### Updated: Missing dialysis time not dropped, assumed missing is no dialysis ###
df_cand_kipa <- df_cand_kipa %>%
  drop_na(CAN_DIAB_TY, CAN_AGE_AT_LISTING, CAN_PREV_TX) %>%
  filter(CAN_DIAB_TY != '998')


### Add living donor indicator variable
df_cand_kipa$living_donor <- 0
df_cand_kipa$living_donor[df_cand_kipa$DON_TY == 'L'] <- 1


### Add certain table variables
df_cand_kipa <- df_cand_kipa %>%
  mutate(
    diabetes_cat = case_when(
      CAN_DIAB_TY == '2' | CAN_DIAB_TY == '3' | CAN_DIAB_TY == '4' | CAN_DIAB_TY == '5' ~ '1',
      TRUE ~ '0'))


### Dialysis time at listing, transplant, preemptive transplant, on dialysis at listing
### Updated: Missing dialysis date is dialysis time of 0 (no dialysis) ###
# sum(is.na(df_cand_kipa$CAN_LISTING_DT))
# sum(is.na(df_cand_kipa$REC_TX_DT))

df_cand_kipa <- df_cand_kipa %>% 
  mutate(
    dialysis_time_at_list = case_when(
      is.na(CAN_DIAL_DT) | as.Date(CAN_LISTING_DT) < as.Date(CAN_DIAL_DT) ~ 0,
      TRUE ~ as.numeric(difftime(CAN_LISTING_DT, CAN_DIAL_DT, units = 'weeks')) / 52.25),
    
    dialysis_time_at_transplant = case_when(
      is.na(CAN_DIAL_DT) | is.na(REC_TX_DT) ~ 0,
      TRUE ~ as.numeric(difftime(REC_TX_DT, CAN_DIAL_DT, units = 'weeks')) / 52.25),
    
    preemptive_transplant = case_when(
      dialysis_time_at_transplant == 0 ~ 1,
      TRUE ~ 0),
    
    preemptive_listing = case_when(
      dialysis_time_at_list == 0 ~ 1,
      TRUE ~ 0),
    
    dialysis = case_when(
      preemptive_listing == 0 ~ 1,
      TRUE ~ 0)
    )
    


################################################
# 2. Add EPTS scores
################################################
### Raw EPTS scores
df_cand_kipa <- df_cand_kipa %>% 
  mutate(raw_epts = 
           0.047*pmax(CAN_AGE_AT_LISTING - 25, 0) - 
           0.015*(diabetes_cat==1)*pmax(CAN_AGE_AT_LISTING - 25, 0) +
           0.398*CAN_PREV_TX - 0.237*(diabetes_cat==1)*CAN_PREV_TX +
           0.315*log(dialysis_time_at_list + 1) - 0.099*(diabetes_cat==1)*log(dialysis_time_at_list + 1) +
           0.130*(dialysis_time_at_list == 0) - 0.348*(dialysis_time_at_list == 0) +  
           1.262*(diabetes_cat==1))

df_cand_kipa$dialysis_time_at_list <- round(df_cand_kipa$dialysis_time_at_list, 1)
df_cand_kipa$dialysis_time_at_transplant <- round(df_cand_kipa$dialysis_time_at_transplant, 1)


### Percentile EPTS scores
df_cand_kipa <- df_cand_kipa %>%
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

df_cand_kipa$top_percentile_epts = factor(df_cand_kipa$top_percentile_epts, levels=c(0,1))



################################################
# 3. Clean variables
################################################
df_cand_kipa <- df_cand_kipa %>%
  select(PX_ID, PERS_ID, DONOR_ID, CAN_GENDER,
         CAN_RACE, CAN_LISTING_DT, CAN_AGE_AT_LISTING, CAN_PREV_TX, PERS_NEXTTX, REC_TX_DT,
         CAN_REM_CD, CAN_REM_DT, CAN_LAST_ACT_STAT_DT, CAN_DEATH_DT, PERS_OPTN_DEATH_DT, PERS_SSA_DEATH_DT,
         diabetes_cat, dialysis,
         dialysis_time_at_list, dialysis_time_at_transplant, preemptive_transplant, preemptive_listing,
         raw_epts, percentile_epts, top_percentile_epts, living_donor)

df_cand_kipa <- expss::drop_all_labels(df_cand_kipa)


### Demographic variables
df_cand_kipa <- df_cand_kipa %>%
  mutate(race = case_when(
    CAN_RACE == '8' ~ 'White',
    CAN_RACE == '16' ~ 'Black_AA',
    CAN_RACE == '32' ~ 'American_Indian',
    CAN_RACE == '64' ~ 'Asian',
    CAN_RACE == '128' ~ 'Pacific_Islander',
    CAN_RACE == '256' ~ 'Arab_Middle_East',
    CAN_RACE == '512' ~ 'Indian_SubContinent',
    CAN_RACE == '1024' ~ 'Unknown',
    CAN_RACE == '2000' ~ 'Hispanic',
    TRUE ~ 'Other')) %>%
  
  mutate(sex = case_when(
    CAN_GENDER == 'M' ~ 'Male',
    CAN_GENDER == 'F' ~ 'Female'),
    
    diabetes = factor(diabetes_cat, levels=c(0,1)),
    dialysis = factor(dialysis, levels=c(0,1)),
    preemptive_transplant = factor(preemptive_transplant, levels=c(0,1)),
    preemptive_listing = factor(preemptive_listing, levels=c(0,1)),
    previous_TX = factor(CAN_PREV_TX, levels=c(0,1)),
    age = as.numeric(CAN_AGE_AT_LISTING)
  ) %>%
  
  mutate(
    age_group = case_when(
      age <= 24 ~ '18-24',
      age > 24 & age <= 29 ~ '25-29',
      age > 29 & age <= 39 ~ '30-39',
      age > 39 & age <= 49 ~ '40-49',
      age > 49 & age <= 59 ~ '50-59',
      age > 59 & age <= 69 ~ '60-69',
      TRUE ~ '>70')) %>%
  
  select(-c(CAN_RACE, CAN_PREV_TX, CAN_AGE_AT_LISTING, diabetes_cat))

df_cand_kipa$age_group <- factor(df_cand_kipa$age_group, levels=c('18-24', '25-29', '30-39',
                                                                  '40-49', '50-59', '60-69', '>70')) 
df_cand_kipa$race <- factor(df_cand_kipa$race, levels=c('White', 'American_Indian', 'Arab_Middle_East',
                                                        'Asian', 'Black_AA', 'Hispanic', 'Indian_SubContinent',
                                                        'Other', 'Pacific_Islander', 'Unknown'))
  

#########################
### Transplant date and transplant time in days
df_cand_kipa <- df_cand_kipa %>%
  
  mutate(transplant = case_when(
    !is.na(PERS_NEXTTX) | !is.na(REC_TX_DT) | CAN_REM_CD == '4' ~ '1',
    TRUE ~ '0')) %>%
      
  mutate(transplant_date = NA_Date_,
         transplant_time = NA)

df_cand_kipa$transplant_date[df_cand_kipa$transplant == '1'] <- 
  pmin(as.Date(df_cand_kipa$PERS_NEXTTX[df_cand_kipa$transplant == '1']), 
       as.Date(df_cand_kipa$REC_TX_DT[df_cand_kipa$transplant == '1']), 
       as.Date(df_cand_kipa$CAN_REM_DT[df_cand_kipa$transplant == '1']), na.rm = T)


df_cand_kipa$transplant_time[df_cand_kipa$transplant == '1'] <- 
  as.numeric(as.Date(df_cand_kipa$transplant_date[df_cand_kipa$transplant == '1']) - 
               as.Date(df_cand_kipa$CAN_LISTING_DT[df_cand_kipa$transplant == '1']),
             units = 'days')
  
# For same day transplants, round up to 1 day of transplant time
df_cand_kipa$transplant_time[df_cand_kipa$transplant_time == '0' & df_cand_kipa$transplant == '1'] <- 1 



#########################
### Death date and time until death (survival time)
df_cand_kipa <- df_cand_kipa %>% 
  
  mutate(death = case_when(
    CAN_REM_CD == '8' | !is.na(CAN_DEATH_DT) | !is.na(PERS_SSA_DEATH_DT) | !is.na(PERS_OPTN_DEATH_DT) ~ '1',
    TRUE ~ '0'))  %>%
  
  mutate(death_date = NA_Date_,
         survival_time = NA,
         death_after_TX = 0)

# Minimum of listed death dates, then maximum of that with removal date since removal can come before death
df_cand_kipa$death_date[df_cand_kipa$death == '1'] <- 
  pmin(as.Date(df_cand_kipa$CAN_DEATH_DT[df_cand_kipa$death == '1']), 
       as.Date(df_cand_kipa$PERS_SSA_DEATH_DT[df_cand_kipa$death == '1']),
       as.Date(df_cand_kipa$PERS_OPTN_DEATH_DT[df_cand_kipa$death == '1']), na.rm = T)

df_cand_kipa$death_date[df_cand_kipa$death == '1'] <- 
  pmax(df_cand_kipa$death_date[df_cand_kipa$death == '1'],
       as.Date(df_cand_kipa$CAN_REM_DT[df_cand_kipa$death == '1']), na.rm = T)


df_cand_kipa$survival_time[df_cand_kipa$death == '1'] <- 
  as.numeric(as.Date(df_cand_kipa$death_date[df_cand_kipa$death == '1']) - 
               as.Date(df_cand_kipa$CAN_LISTING_DT[df_cand_kipa$death == '1']),
             units = 'days')
# For same day deaths, round up to 1 day of survival time
df_cand_kipa$survival_time[df_cand_kipa$survival_time == '0' & df_cand_kipa$death == '1'] <- 1 


#########################
### Death after transplant (survival time >= transplant time)
df_cand_kipa$death_after_TX[df_cand_kipa$survival_time >= df_cand_kipa$transplant_time &
                            df_cand_kipa$transplant == '1' &
                            df_cand_kipa$death == '1'] <- 1

# Transplant takes priority over death
df_cand_kipa$death[df_cand_kipa$death_after_TX == '1'] <- 0



#########################
### Time: transplant time or death time, or time until removal
df_cand_kipa <- df_cand_kipa %>%
  mutate(time = case_when(
    transplant == '1' ~ transplant_time,
    death == '1' ~ survival_time,
    TRUE ~ as.numeric(as.Date(CAN_REM_DT) - as.Date(CAN_LISTING_DT), units = 'days')))

# Fill with last active status date if still missing endpoint date
df_cand_kipa$time[is.na(df_cand_kipa$time) & !is.na(df_cand_kipa$CAN_LAST_ACT_STAT_DT)] <- 
  as.numeric(as.Date(df_cand_kipa$CAN_LAST_ACT_STAT_DT[is.na(df_cand_kipa$time) & 
                                                         !is.na(df_cand_kipa$CAN_LAST_ACT_STAT_DT)]) - 
               as.Date(df_cand_kipa$CAN_LISTING_DT[is.na(df_cand_kipa$time) & 
                                                     !is.na(df_cand_kipa$CAN_LAST_ACT_STAT_DT)]), 
             units = 'days')
  


################################################
# 4. Add kipa scores and finalize candidate dataset
################################################
### Read in kdpi scores from Jessica's dataset
df_kdpi <- read.csv('./fulldata.csv')
df_kdpi <- df_kdpi[ ,c('DONOR_ID', 'kdpi')]
df_kdpi <- df_kdpi[!duplicated(df_kdpi), ]


df_cand_kipa <- merge(df_cand_kipa, df_kdpi, by = 'DONOR_ID', all.x = T, all.y = F)


### Indicator variable for top 20 KDPI/not top 20 KDPI for those with transplant
df_cand_kipa <- df_cand_kipa %>%
  mutate(
    top20_kdpi = case_when(
      transplant == '1' & kdpi <= 20 & !is.na(kdpi) ~ '1',
      TRUE ~ '0'),
    
    not_top20_kdpi = case_when(
      transplant == '1' & kdpi > 20 & !is.na(kdpi) ~ '1',
      TRUE ~ '0')) %>%
  
  mutate(
    censored = case_when(
      death == '0' & transplant == '0' ~ '1',
      TRUE ~ '0')) %>%
  
  mutate(top20_kdpi = factor(top20_kdpi, levels=c(0,1)),
         not_top20_kdpi = factor(not_top20_kdpi, levels=c(0,1)),
         censored = factor(censored, levels=c(0,1)))


df_cand_kipa <- df_cand_kipa[ ,c('PX_ID', 'PERS_ID', 'raw_epts', 'percentile_epts', 'top_percentile_epts',
                                 'kdpi', 'top20_kdpi', 'not_top20_kdpi', 
                                 'time', 'CAN_LISTING_DT',
                                 'death', 'death_date', 'death_after_TX',
                                 'transplant', 'transplant_date', 'transplant_time',
                                 'censored', 'CAN_REM_CD', 'CAN_REM_DT', 'previous_TX',  
                                 'age', 'age_group', 'race', 'sex', 'diabetes', 'dialysis',
                                 'dialysis_time_at_list', 'dialysis_time_at_transplant', 
                                 'preemptive_transplant', 'preemptive_listing', 'living_donor')]

colnames(df_cand_kipa) <- c('PX_ID', 'PERS_ID', 'raw_epts', 'percentile_epts', 'top_percentile_epts',
                            'kdpi', 'top20_kdpi', 'not_top20_kdpi',
                            'time', 'listing_date',
                            'death', 'death_date', 'death_after_TX',
                            'transplant', 'transplant_date', 'transplant_time',
                            'censored', 'removal_code', 'removal_date', 'previous_TX',  
                            'age', 'age_group', 'race', 'sex', 'diabetes', 'dialysis',
                            'dialysis_time_at_list', 'dialysis_time_at_transplant', 
                            'preemptive_transplant', 'preemptive_listing', 'living_donor')



### Remove candidates with missing time (~3%)
df_cand_kipa <- subset(df_cand_kipa, !is.na(df_cand_kipa$time))



save(df_cand_kipa, file = './candidates_livingdonor.RData')
write.csv(df_cand_kipa, file = './candidates_livingdonor.csv')



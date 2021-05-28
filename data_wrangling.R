#!/usr/bin/env Rscript

require(dplyr)
require(reshape2)
require(tidyr)

### MARS dataset
# 7009 unique patient ID's
# 7416 unique hospital admissions
# 8332 unique ICU admissions
# 4877 unique sepsis episodes
  

### Load admissions ~ assessment dates
# Assessment_dates_TK not present in sofa_scores: 146960
ass_dates <- haven::read_sas(paste0(data_directory,"assessment_dates.sas7bdat"))[,c(1,2,4,8)] # admissions and dates
ass_dates <- ass_dates %>%
  mutate(Assessment_date = as.Date(as.POSIXct(Assessment_date, origin = '1960-01-01', tz = "UTC"))) %>%
  mutate(Index_ICU_admittance_datetime = as.Date(as.POSIXct(Index_ICU_admittance_datetime,
                                                            origin = '1960-01-01', tz = "UTC")))

### Load admissions ~ discharge dates
discharge_dates <- haven::read_sas(paste0(data_directory,"icu_admission.sas7bdat"))[,c(1,8,13)]
discharge_dates <-
  discharge_dates %>%
  mutate(Index_ICU_discharge_datetime = as.Date(as.POSIXct(Index_ICU_discharge_datetime,
                                                           origin = '1960-01-01', tz = "UTC")))

### Load assessment dates ~ SOFA scores
sofa_scores <- haven::read_sas(paste0(data_directory,"daily_sepsis_rel_organ_fail.sas7bdat"))[,c(2,5:13)]
# Non-unique assessment_dates_TK: 89789 on rows 16374 and 16375
sofa_scores <- sofa_scores[-16374,] # Empty duplicate removed

# Convert SOFA-scores to integers
score_to_int <- function(x){
  score <- as.integer(substring(as.character(x),1,1))
  return(score)
}

sofa_scores[2:10] <- apply(sofa_scores[2:10], 2, score_to_int)
sofa_scores[sofa_scores == 9] <- NA # Note: 9 converts to 'unkown' (GIS-score/CAM_ICU) or 'unobservable due to sedation' (CNS_SOFA)
sofa_scores$SOFA_Total <- rowSums(sofa_scores[3:8], na.rm = TRUE)

## Merge admissions with assessment dates and SOFA scores
sofa_date_admission <-
  ass_dates %>%
  inner_join(sofa_scores, by = "Assessment_Dates_TK")

## Filter admissions with SOFA-score >=2 on any assessment date
min_SOFA_two <-
  sofa_date_admission %>%
  group_by(ICU_Admission_TK) %>%
  summarise(min_SOFA = min(SOFA_Total)) %>%
  filter(min_SOFA >=2)

### Load admissions ~ sepsis episodes
sepsis_episodes <- haven::read_sas(paste0(data_directory,"episode_of_sepsis.sas7bdat"))[,c(1,2,4,5,7)]
sepsis_episodes <- sepsis_episodes %>%
  mutate(Sepsis_startdate = as.Date(as.POSIXct(Sepsis_startdate, origin = '1960-01-01', tz = "UTC")))
  
## Filter episodes with probable and/or culture proven infection
# 2667 episodes, 2210 episodes filtered out
true_episodes <- sepsis_episodes %>%
  filter(Infection_likelihood_primary == 'probable' |
            Infection_likelihood_primary == 'culture proven'|
            Infection_likelihood_secondary == 'probable' |
            Infection_likelihood_secondary == 'culture proven') %>%
  select(ICU_Admission_TK,Episode_Of_Sepsis_TK,Sepsis_startdate)
  
### Definitive filter
# Admissions with >= 1 probable/culture proven sepsis episode AND SOFA-score >=2 on any assessment date
selection_filter <- intersect(true_episodes$ICU_Admission_TK, min_SOFA_two$ICU_Admission_TK) # 1462 admissions meet criteria

### Further addition of relevant info
admission_info <- haven::read_sas(paste0(data_directory,"ICU_Admission.sas7bdat"))[,c(1,14)]

treatments_events <- haven::read_sas(paste0(data_directory,"daily_treatments_and_events.sas7bdat"))[,c(1,5,12,15,16)]
treatments_events <-
  treatments_events %>%
  mutate(CAM_ICU_delirium_score = score_to_int(CAM_ICU_delirium_score)) %>%
  mutate(CAM_ICU_delirium_score = case_when(CAM_ICU_delirium_score == 9 ~ NA_real_,
                                            TRUE ~ as.numeric(CAM_ICU_delirium_score)))

# PaO2_at_highest_A_aDO2 and/or PaO2, Creatinin_Max, Bilirubin_Max, Platelet_count_Min (/or Max)  
daily_lab <- haven::read_sas(paste0(data_directory,"daily_standard_lab_results.sas7bdat"))[c(2,10,14,19,27,32,36)]

# ABPm_Min; consider adding dopamine (from drug_data_v2.R)
daily_HD <- haven::read_sas(paste0(data_directory,"daily_monitor_data_hemodynamic.sas7bdat"))[c(2,15)]

# FiO2 and/or FiO2_at_highest_A_aDo2
daily_resp <- haven::read_sas(paste0(data_directory,"daily_monitor_data_respiratory.sas7bdat"))[c(2,8,13)]

# Baseline values 
baseline_TKs <- haven::read_sas(paste0(data_directory, "baseline_period.sas7bdat"))[c(1,2)]
baseline_lab <- haven::read_sas(paste0(data_directory, "baseline_laboratory_results.sas7bdat"))[c(1,8,13,16,23)]
baseline_vars <- haven::read_sas(paste0(data_directory, "baseline_monitor_data.sas7bdat"))[c(1,11,15)]

new_baseline_varnames <- c('PaO2_at_highest_A_aDO2', 'Platelet_count_Min', 
                           'Creatinin_Max', 'Bilirubin_Max', 'ABPm_Min',
                           'FiO2_at_highest_A_aDO2')

baseline_TKs %>%
  filter(ICU_Admission_TK %in% selection_filter) %>%
  left_join(baseline_lab) %>%
  left_join(baseline_vars) %>%
  mutate(days_since_admission = rep(0,nrow(.))) %>%
  select(-Baseline_Period_TK) %>%
  rename_at(vars(PaO2_at_highest_A_aDO2_24h:FiO2_at_highest_A_aDO2_24h), 
            ~!!new_baseline_varnames) -> baseline_data

# Method for inserting rows into subset of columns
replace_subset <- function(df, df_subset, id_col_names = c()) {
  
  # work out which of the columns contain "new" data
  new_data_col_names <- colnames(df_subset)[which(!colnames(df_subset) %in% id_col_names)]
  
  # complete the df_subset with the extra columns from df
  df_sub_to_join <- df_subset %>%
    left_join(select(df, -new_data_col_names), by = c(id_col_names))
  
  # join and bind rows
  df_out <- df %>%
    anti_join(df_sub_to_join, by = c(id_col_names)) %>%
    bind_rows(df_sub_to_join)
  
  return(df_out)
}

# Select correct admissions with definitive filter and merge datasets
## Note on duplicate assessment dates/sepsis episodes
# 3511 assessment dates appear in duplicates/triplicates/quadruplicates. 
# These dates are all from admissions (N=178) with multiple sepsis episodes. 
# Of course for overlapping episodes only a single SOFA score daily is recorded. 
# Hence, only a single episode is considered per patient.
sofa_episodes <-
  sofa_date_admission %>%
  filter(ICU_Admission_TK %in% selection_filter) %>% # Filter correct admissions
  left_join(true_episodes) %>% # Merge admissions and daily sofa scores with sepsis episodes
  left_join(daily_lab) %>%
  left_join(daily_HD) %>%
  left_join(daily_resp) %>%
  group_by(ICU_Admission_TK, Episode_Of_Sepsis_TK) %>%
  mutate(days_since_admission = Assessment_date - Index_ICU_admittance_datetime,
          days_since_start_sepsis = Assessment_date - Sepsis_startdate) %>% # Add days elapsed columns
  ungroup() %>%
  group_by(ICU_Admission_TK) %>%
  distinct(Assessment_Dates_TK, .keep_all=TRUE) %>% # See note below. 
  select(ICU_Admission_TK, Episode_Of_Sepsis_TK, # Rearrange columns
          Assessment_Dates_TK, Index_ICU_admittance_datetime, Sepsis_startdate, 
          Assessment_date, days_since_admission, days_since_start_sepsis, everything()) %>%
  arrange(ICU_Admission_TK, Episode_Of_Sepsis_TK, Assessment_Dates_TK) %>% # Sort data
  filter(!(Episode_Of_Sepsis_TK %in% c(16413, 11233))) %>% # Incorrect sepsis startdates
  filter(!(Assessment_Dates_TK %in% c(6964, 6965))) %>% # Incorrect assessment date
  left_join(admission_info) %>%
  left_join(treatments_events) %>%
  left_join(discharge_dates, by = 'ICU_Admission_TK') %>%
  group_by(ICU_Admission_TK) %>%
  mutate(LoS = ifelse(max(Assessment_date) > Index_ICU_discharge_datetime,
                      as.numeric(max(Assessment_date) - Index_ICU_admittance_datetime),
                      as.numeric(Index_ICU_discharge_datetime - Index_ICU_admittance_datetime))) %>%
  complete(Assessment_date = seq.Date(unique(Index_ICU_admittance_datetime), 
                                           unique(Index_ICU_discharge_datetime), by="day")) %>%
  mutate(days_since_admission = 0:max(LoS, na.rm=TRUE)) %>%
  ungroup() %>%
  replace_subset(baseline_data, id_col_names = c('ICU_Admission_TK', 'days_since_admission')) %>%
  select(-c(Episode_Of_Sepsis_TK, Assessment_Dates_TK, DIC_score, Acute_kidney_injury_score, 
            SOFA_Total, Lactate_Max, PaO2, FiO2, CAM_ICU_delirium_score, Index_ICU_admittance_datetime, Sepsis_startdate,
                 days_since_start_sepsis, Index_ICU_discharge_datetime, LoS))

### Next, join drug- and fluid tibbles then join result with SOFA_scores etc.
source_dir <- '/Users/emielpost/Dropbox/BS_ML/MODS/'
source(paste0(source_dir,'fluid_data.R'))
source(paste0(source_dir,'drug_data.R'))

new_varnames <- c('Days_elapsed','CNS','Circ','Coag','Liver','Renal','Resp','GIS','bili','creat',
                  'PaO2Aa','Platelets','MAPmin','FiO2Aa','survival','UO','MV','RRT',
                  'Albumin','Blood','Colloids','Crystalloids','Ent','Parent','dobutamine','erytromycine',
                  'furosemide','hydrocortison','midazolam','nor','propofol','opioids','VP','pF','nor_gamma')

####### final adjustments #######
sofa_episodes %>%
  left_join(fluid_data_final, by = c('ICU_Admission_TK', 'Assessment_date')) %>%
  left_join(drug_data_final, by = c('ICU_Admission_TK', 'Assessment_date')) %>%
  mutate(across(Albumin:vasopressine, ~replace(., is.na(.), 0))) %>%
  mutate(pF = PaO2_at_highest_A_aDO2/(FiO2_at_highest_A_aDO2/100)) %>%
  mutate(Patient_weight = case_when(is.na(Patient_weight) ~ 78.7, # Average weight in dataset
         TRUE ~ as.numeric(Patient_weight))) %>%
  mutate(avg_nor_gamma = norepinefrine/24/60/Patient_weight*1000) %>% # Calculate avg nor-dose for SOFA circulation
  select(-c(Assessment_date, Patient_weight)) %>%
  rename_at(vars(days_since_admission:avg_nor_gamma), ~!!new_varnames) %>%
  group_by(ICU_Admission_TK) %>%
  select(-CNS) %>% # CNS excluded!
  filter(Days_elapsed != 0) %>%
  mutate(Liver = case_when((is.na(Liver) & bili < 20) ~ 0,
                                                       (is.na(Liver) & (bili >= 20 & bili < 33)) ~ 1,
                                                       (is.na(Liver) & (bili >= 33 & bili < 102)) ~ 2,
                                                       (is.na(Liver) & (bili >= 102 & bili < 205)) ~ 3, 
                                                       (is.na(Liver) & bili >= 205 ~ 4),
                                                       TRUE ~ as.numeric(Liver))) %>%
  mutate(Coag = case_when((is.na(Coag) & Platelets > 150) ~ 0,
                          (is.na(Coag) & (Platelets > 100 & Platelets <= 150)) ~ 1,
                          (is.na(Coag) & (Platelets > 50 & Platelets <= 100)) ~ 2,
                          (is.na(Coag) & (Platelets > 20 & Platelets <= 50)) ~ 3,
                          (is.na(Coag) & Platelets <= 20 ~ 4),
                          TRUE ~ as.numeric(Coag))) %>%
  mutate(Resp = case_when((is.na(Resp) & pF > 400) ~ 0,
                          (is.na(Resp) & (pF > 300 & pF <= 400)) ~ 1,
                          (is.na(Resp) & (pF > 200 & pF <= 300)) ~ 2,
                          (is.na(Resp) & (pF > 100 & pF <= 200) & MV == 1) ~ 3,
                          (is.na(Resp) & pF <= 100 & MV == 1) ~ 4,
                          TRUE ~ as.numeric(Resp))) %>%
  mutate(Renal = case_when((is.na(Renal) & creat <= 110) ~ 0,
                           (is.na(Renal) & (creat > 110 & creat <= 170)) ~ 1,
                           (is.na(Renal) & (creat > 170 & creat <= 299)) ~ 2,
                           (is.na(Renal) & ((UO >= 200 & UO < 500)|(creat > 300 & creat <= 440))) ~ 3,
                           (is.na(Renal) & (UO < 200 | creat > 440) ~ 4),
                           TRUE ~ as.numeric(Renal))) %>%
  mutate(Circ = case_when((is.na(Circ) & (nor == 0 & dobutamine == 0 & MAPmin >= 70)) ~ 0,
                          (is.na(Circ) & (nor == 0 & dobutamine == 0 & MAPmin < 70)) ~ 1,
                          (is.na(Circ) & (nor == 0 & dobutamine > 0)) ~ 2,
                          (is.na(Circ) & (nor > 0 & nor <= 0.1)) ~ 3,
                          (is.na(Circ) & (nor > 0.1) ~ 4),
                          TRUE ~ as.numeric(Circ))) %>%
  filter_at(vars(Circ:GIS), all_vars(complete.cases(.))) -> sdf # Filter out rows with all SOFA-scores missing; almost exclusively last day of stay

dir2writeto <- '/Users/emielpost/Dropbox/BS_ML/Scripts/Data/'
write.csv(sdf, paste0(dir2writeto,"data_sepsis_wo_CNS_d0.csv"), row.names=FALSE)


  





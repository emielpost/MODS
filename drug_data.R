#!/usr/bin/env Rscript

# Extract drug data based on background-knowledge and abundance of use:
# Sedatives/analgetics: propofol, midazolam and synthetic opioids; inotropics: dobutamine; 
# vasopressors: norepinefrin, vasopressin (terli- and arginine vasopressin); diuretics: furosemide; 
# steroids: hydrocortisone; prokinetics: erythromycin.

data_directory <- '/Users/emielpost/Dropbox/BS_ML/Scripts/Data/DataDeliveryMarsAMC20190621/'

drug_data <- haven::read_sas(paste0(data_directory, "event_medications.sas7bdat"))[,c(1,2,4:6,9)]
drug_data %>%
  filter(ICU_Admission_TK %in% unique(sofa_episodes['ICU_Admission_TK'][[1]])) -> drug_data
admission_data <- haven::read_sas(paste0(data_directory,"icu_admission.sas7bdat"))[,c(1,8)] # Patient weight
drug_data <- left_join(drug_data, admission_data, by = 'ICU_Admission_TK') 

drugs2select <- c("alfentanil","Alfentanil","Fentanyl", "remifentanil", "Remifentanil", "sufentanil", 
                  "Sufentanil", "dobutamine", "Dobutamine", "furosemide", "Furosemide", "Furosemide Retard tbl", 
                  "hydrocortison", "Hydrocortison", "norepinefrine", "Norepinefrine", "Norepinefrine 4-voudig", 
                  "Norepinefrine d.d.", "propofol", "Propofol", "Propofol TCI", "Hydrocortison", "hydrocortison", 
                  "Arginine vasopressine", "Midazolam", "midazolam", 'erytromycine', 'Erythromycine', "terlipressine", "Terlipressine")

drug_data %>%
  filter(medication_generic_drug_name %in% !!drugs2select) %>%
  mutate(Medication_startdatetime = as.POSIXct(Medication_startdatetime, origin = '1960-01-01', tz = "UTC")) %>%
  mutate(medication_stopdatetime = as.POSIXct(medication_stopdatetime, origin = '1960-01-01', tz = "UTC")) %>%
  mutate(infusion_time = difftime(medication_stopdatetime, Medication_startdatetime, units = 'min'),
         dose_per_minute = medication_dose/as.integer(infusion_time),
         dose_kg_min = dose_per_minute/Patient_weight) %>%
  mutate(medication_generic_drug_name = case_when(medication_generic_drug_name %in% c("alfentanil","Alfentanil","Fentanyl",
                                                                                      "remifentanil", "Remifentanil",
                                                                                      "sufentanil","Sufentanil") ~ 'synthetic_opioids',
                                                  medication_generic_drug_name %in% c("dobutamine", "Dobutamine") ~ "dobutamine",
                                                  medication_generic_drug_name %in% c("furosemide", "Furosemide", 
                                                                                      "Furosemide Retard tbl") ~ "furosemide",
                                                  medication_generic_drug_name %in% c("hydrocortison", "Hydrocortison") ~ "hydrocortison",
                                                  medication_generic_drug_name %in% c("norepinefrine","Norepinefrine",
                                                                                      "Norepinefrine 4-voudig", 
                                                                                      "Norepinefrine d.d.") ~ "norepinefrine",
                                                  medication_generic_drug_name %in% c("propofol", "Propofol", "Propofol TCI") ~ "propofol",
                                                  medication_generic_drug_name %in% c("Hydrocortison", "hydrocortison") ~ "hydrocortison",
                                                  medication_generic_drug_name %in% c("Arginine vasopressine", 'terlipressine', 'Terlipressine') ~ "vasopressine",
                                                  medication_generic_drug_name %in% c("Midazolam", "midazolam") ~ "midazolam",
                                                  medication_generic_drug_name %in% c('erytromycine', 'Erythromycine') ~ 'erytromycine')) %>%
  mutate(Medication_startdatetime = as.Date(Medication_startdatetime)) %>%
  mutate(medication_stopdatetime = as.Date(medication_stopdatetime)) -> drug_data 

drug_data %>%
  mutate(dose_per_day = medication_dose/(as.integer(medication_stopdatetime - Medication_startdatetime) + 1)) -> drug_data

drug_data %>%
  select(ICU_Admission_TK, Medication_startdatetime, medication_stopdatetime, 
         medication_generic_drug_name, dose_per_day) %>%
  # sequence of dates for each corresponding start date, end date element
  mutate(ICU_Admission_TK, Assessment_date = map2(Medication_startdatetime, medication_stopdatetime, seq, by = "1 day")) %>%
  # unnest the list column
  unnest(cols = c(Assessment_date)) %>% 
  # remove any duplicate rows
  distinct %>%
  select(ICU_Admission_TK, Assessment_date, 
         medication_generic_drug_name, dose_per_day) %>%
  group_by(ICU_Admission_TK, Assessment_date, medication_generic_drug_name) %>%
  summarise(avg_dose = sum(dose_per_day)) %>%
  spread(medication_generic_drug_name, avg_dose) %>%
  replace(is.na(.), 0) -> drug_data_final



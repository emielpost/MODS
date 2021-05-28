#!/usr/bin/env Rscript

# Extract fluid data based on background-knowledge
library(dplyr)
library(purrr)

data_directory <- '/Users/emielpost/Dropbox/BS_ML/Scripts/Data/DataDeliveryMarsAMC20190621/'
fluid_data <- haven::read_sas(paste0(data_directory, "event_fluids_feedings_transf.sas7bdat"))[,c(1,2,4:8)]

sofa_episodes %>%
  distinct(ICU_Admission_TK) %>%
  pull(ICU_Admission_TK) -> admissions

fluid_data %>%
  filter(ICU_Admission_TK %in% unique(admissions)) %>%
  mutate(Fluid_generic_name = case_when(Fluid_generic_name %in% c("Cristalloids, balanced solution", 
                                                                  "Cristalloids, glucose 5%",
                                                                  "Cristalloids, hypertonic glucose",
                                                                  "Cristalloids, hypertonic saline",
                                                                  "Cristalloids, mannitol",
                                                                  "Cristalloids, normal saline",
                                                                  "Cristalloids, other") ~ "Crystalloids",
                                        Fluid_generic_name %in% c("Autologous whole blood", 
                                                                  "Erythrocyte concentrate",
                                                                  "Fresh frozen plasma",
                                                                  "Thrombocyte concentrate") ~ "Blood_products",
                                        Fluid_generic_name %in% c("Nutrition, enteral", 
                                                                  "Nutrition, oral diet") ~ "Nutrition_enteral",
                                        Fluid_generic_name %in%  c("Colloids, starch") ~ "Colloids",
                                        Fluid_generic_name %in% c("Albumin 20%") ~ "Albumin",
                                        Fluid_generic_name %in% c('Nutrition, parenteral') ~ 'Nutrition_parenteral')) %>%
  mutate(Fluid_startdatetime = as.POSIXct(Fluid_startdatetime, origin = '1960-01-01', tz = "UTC")) %>%
  mutate(Fluid_stopdatetime = as.POSIXct(Fluid_stopdatetime, origin = '1960-01-01', tz = "UTC")) %>%
  mutate(Fluid_startdatetime = as.Date(Fluid_startdatetime)) %>%
  mutate(Fluid_stopdatetime = as.Date(Fluid_stopdatetime)) %>%
  filter(!is.na(Fluid_generic_name)) -> fluid_data

fluid_data %>%
  mutate(dose_per_day = Fluid_dose/(as.integer(Fluid_stopdatetime - Fluid_startdatetime) + 1)) -> fluid_data

fluid_data %>%
  select(ICU_Admission_TK, Fluid_startdatetime, Fluid_stopdatetime, 
         Fluid_generic_name, dose_per_day) %>%
  # sequence of dates for each corresponding start date, end date element
  mutate(ICU_Admission_TK, Assessment_date = map2(Fluid_startdatetime, Fluid_stopdatetime, seq, by = "1 day")) %>%
  # unnest the list column
  unnest(cols = c(Assessment_date)) %>% 
  # remove any duplicate rows
  distinct %>%
  select(ICU_Admission_TK, Assessment_date, 
         Fluid_generic_name, dose_per_day) %>%
  group_by(ICU_Admission_TK, Assessment_date, Fluid_generic_name) %>%
  summarise(avg_dose = sum(dose_per_day)) %>%
  spread(Fluid_generic_name, avg_dose) %>%
  replace(is.na(.), 0) -> fluid_data_final






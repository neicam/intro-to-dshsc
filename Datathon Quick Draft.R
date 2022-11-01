library(tidyverse)
library(janitor)
library(patchwork)

#Reading in data
Orig_healthutil_raw <- read_csv("OECDhealthutil.csv")
Orig_healthresource_raw <- read_csv("OECDhealthresource.csv")

#Tidy data to wider format
Orig_healthutil_tidy <- Orig_healthutil_raw %>%
  #reformat variable names to be all be in snake_case
  clean_names() %>% 
  pivot_wider(names_from = c(variable, measure),
              values_from = value,
              id_cols = c(year, country))

Orig_healthresource_tidy <- Orig_healthresource_raw %>% 
  clean_names() %>% 
  pivot_wider(names_from = c(variable, measure),
              values_from = value,
              id_cols = c(year, country))

#Tidying names again
Orig_healthresource_tidynames <- Orig_healthresource_tidy %>%
  clean_names()
Orig_healthutil_tidynames <- Orig_healthutil_tidy %>% 
  clean_names()

#Selecting out relevant columns
Orig_healthresource_doctors <- Orig_healthresource_tidynames %>% 
  select(year, country, medical_graduates_number, medical_graduates_per_100_000_population, practising_physicians_number_of_persons_head_counts, practising_physicians_density_per_1_000_population_head_counts) %>% 
  mutate(year = as.character(year)) %>% 
  filter(country == 'Australia' | country == 'New Zealand' | country == 'Ireland' | country == 'United Kingdom')
  
Orig_healthutil_waiting <- Orig_healthutil_tidynames %>% 
  mutate(cataract_percent_waiting_3_months = coalesce(cataract_surgery_waiting_times_from_specialist_assessment_to_treatment_percent_of_all_patients_waiting_more_than_3_months, cataract_surgery_waiting_times_of_patients_on_the_list_percent_of_all_patients_waiting_more_than_3_months)) %>% 
  mutate(hip_replacement_waiting_3_months = coalesce(hip_replacement_total_and_partial_including_the_revision_of_hip_replacement_waiting_times_from_specialist_assessment_to_treatment_percent_of_all_patients_waiting_more_than_3_months, hip_replacement_total_and_partial_including_the_revision_of_hip_replacement_waiting_times_of_patients_on_the_list_percent_of_all_patients_waiting_more_than_3_months)) %>% 
  mutate(knee_replacement_waiting_3_months = coalesce(knee_replacement_waiting_times_from_specialist_assessment_to_treatment_percent_of_all_patients_waiting_more_than_3_months, knee_replacement_waiting_times_of_patients_on_the_list_percent_of_all_patients_waiting_more_than_3_months)) %>% 
  mutate(prostatectomy_waiting_3_months = coalesce(prostatectomy_waiting_times_from_specialist_assessment_to_treatment_percent_of_all_patients_waiting_more_than_3_months, prostatectomy_waiting_times_of_patients_on_the_list_percent_of_all_patients_waiting_more_than_3_months)) %>% 
  mutate(coronary_bypass_waiting_3_months = coalesce(coronary_bypass_waiting_times_from_specialist_assessment_to_treatment_percent_of_all_patients_waiting_more_than_3_months, coronary_bypass_waiting_times_of_patients_on_the_list_percent_of_all_patients_waiting_more_than_3_months)) %>% 
  mutate(coronary_angio_waiting_3_months = coalesce(percutaneous_transluminal_coronary_angioplasty_ptca_waiting_times_from_specialist_assessment_to_treatment_percent_of_all_patients_waiting_more_than_3_months, percutaneous_transluminal_coronary_angioplasty_ptca_waiting_times_of_patients_on_the_list_percent_of_all_patients_waiting_more_than_3_months)) %>% 
  mutate(hysterectomy_waiting_3_months = coalesce(hysterectomy_waiting_times_from_specialist_assessment_to_treatment_percent_of_all_patients_waiting_more_than_3_months, hysterectomy_waiting_times_of_patients_on_the_list_percent_of_all_patients_waiting_more_than_3_months)) %>% 
  select(year, country, cataract_percent_waiting_3_months, hip_replacement_waiting_3_months, knee_replacement_waiting_3_months, prostatectomy_waiting_3_months, coronary_bypass_waiting_3_months, coronary_angio_waiting_3_months, hysterectomy_waiting_3_months) %>% 
  mutate(year = as.character(year)) %>% 
  filter(country == 'Australia' | country == 'New Zealand' | country == 'Ireland' | country == 'United Kingdom')
  
#Joining utility and resource databases
Combined_datasets <- full_join(Orig_healthresource_doctors, Orig_healthutil_waiting)

#Medical Graduates Plot
Combined_datasets %>% 
  ggplot(aes(x = year, y = medical_graduates_per_100_000_population, group = country, colour = country)) +
  geom_line() +
  labs(title = 'Medical Graduates per Year', x = 'Year', y = "Graduates per 100 000 people") + 
  scale_colour_discrete(name = 'Country', breaks = c('Ireland', 'Australia', 'United Kingdom', 'New Zealand')) +
  theme(legend.key.size = unit(0.25, 'cm'))

#Waiting times plot
Combined_datasets %>% 
  select(year, country, cataract_percent_waiting_3_months, hip_replacement_waiting_3_months, knee_replacement_waiting_3_months, prostatectomy_waiting_3_months, coronary_bypass_waiting_3_months, coronary_angio_waiting_3_months, hysterectomy_waiting_3_months) %>% 
  pivot_longer(cols = cataract_percent_waiting_3_months:hysterectomy_waiting_3_months, names_to = 'operation', values_to = 'percent') %>% 
  ggplot(aes(x = year, y = percent, group = operation, colour = operation)) +
  geom_line() +
  facet_wrap(vars(country)) +
  theme(legend.key.size = unit(0.2, 'cm'))

  
  


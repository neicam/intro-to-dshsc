library(tidyverse)
library(janitor)

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

#Selecting out relevant columns
Orig_healthresource_doctors <- Orig_healthresource_tidynames %>% 
  select(year, country, medical_graduates_number, medical_graduates_per_100_000_population, practising_physicians_number_of_persons_head_counts, practising_physicians_density_per_1_000_population_head_counts) %>% 
  mutate(year = as.character(year))

#Choosing Australia, UK, Canada and Ireland for example comparison
Orig_healthresource_doctors_compare1 <- Orig_healthresource_doctors %>% 
  filter(country == 'Australia' | country == 'Canada' | country == 'Ireland' | country == 'United Kingdom')

#Quick plot
Orig_healthresource_doctors_compare1 %>% 
  ggplot(aes(x = year, y = medical_graduates_per_100_000_population, group = country, colour = country)) +
  geom_line()



  
  


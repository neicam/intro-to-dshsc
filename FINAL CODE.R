library(tidyverse)
library(prettydoc)  
library(rmdformats) 
library(gapminder) 
library(knitr) 
library(kableExtra) 
library(maps)
library(viridis)
library(stringr)
library(rworldmap)

#Reading in data
Orig_healthutil_raw <- read_csv("OECDhealthutil.csv")
Orig_medicalgraduates_raw <- read_csv("OECDMedGrads.csv")
Orig_doctors_raw <- read_csv("OECDDoctors.csv")

#Simplifying variable names in utilisation dataset
Orig_healthutil_raw <- Orig_healthutil_raw %>% 
  mutate(Variable = str_replace(Variable, "Hip replacement (total and partial, including the revision of hip replacement)", "Hip Replacement"))

#Plot of Hip Replacement waiting times per country
Orig_healthutil_raw  %>% 
  select(Variable,
         Measure,
         Country, 
         Year,
         Value) %>% 
  filter(Measure == "Waiting times from specialist assessment to treatment: Mean (days)",
         Variable == "Hip replacement (total and partial, including the revision of hip replacement)") %>% 
  ggplot(aes(x = Year, y = Value, )) + 
  geom_line() +
  geom_point() +
  facet_wrap(~Country) +
  labs(title = "Waiting times for Hip Procedures", 
       y = "Mean waiting times from assessment to treatment (days)") +
  theme(plot.title=element_text(hjust=0.5), axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1), plot.background = element_rect(fill = "lightblue")) 

# Wrangling data for doctors and graduates and combining to single dataset
Orig_medicalgraduates_raw <- Orig_medicalgraduates_raw %>% 
  mutate(graduatesper1000 = Value/100)

joinedgraduatesanddoctors <- 
  full_join(Orig_medicalgraduates_raw, Orig_doctors_raw, by = c("LOCATION"))

joinedgraduatesanddoctors <- joinedgraduatesanddoctors %>% 
  mutate(Doctors_and_Graduates_per_1000 = graduatesper1000+Value.y) %>% 
  filter(TIME.y == "2020", TIME.x == "2020")

doctors_2020_map <- joinedgraduatesanddoctors %>% 
  select(LOCATION, Doctors_and_Graduates_per_1000)

mapworld <- countryExData %>%
  full_join(doctors_2020_map, by = c("ISO3V10" = "LOCATION"))

#World Map
sPDF <- joinCountryData2Map(mapworld
                            , joinCode = "ISO3"
                            , nameJoinColumn = "ISO3V10")
mapCountryData(sPDF
               , nameColumnToPlot="Doctors_and_Graduates_per_1000" 
               , mapTitle="Doctors and Medical Graduates per 1000 of Population in 2020"
               , oceanCol='lightblue'
               , missingCountryCol='white') 

#Europe Map
sPDF <- joinCountryData2Map(mapworld
                            , joinCode = "ISO3"
                            , nameJoinColumn = "ISO3V10")

mapCountryData(sPDF
               , nameColumnToPlot="Doctors_and_Graduates_per_1000" 
               , mapTitle="Doctors and Medical Graduates per 1000 of Population in 2020"
               , oceanCol='lightblue'
               , missingCountryCol='white'
               , mapRegion='Europe')

#Producing Comparison Table
HIP_OECD_data <- Orig_healthutil_raw %>% 
  filter(Year == "2020", 
         Measure == "Waiting times from specialist assessment to treatment: Mean (days)",
         Variable == "Hip replacement (total and partial, including the revision of hip replacement)")

joinedfortablehip <-
  full_join(HIP_OECD_data, doctors_2020_map, by = c("COU" = "LOCATION"))

agg_tbl <- joinedfortablehip %>% 
  group_by(Country) %>% 
  summarise(mean_Value=mean(Value),
            .groups = 'drop')

agg_tbl2 <- joinedfortablehip %>% 
  group_by(Country) %>% 
  summarise(mean_Doctors_and_Graduates_per_1000=mean(Doctors_and_Graduates_per_1000),
            .groups = 'drop')

joinedfortablehip22 <- 
  full_join(agg_tbl, agg_tbl2, by = c("Country"))

joinedfortablehip22 %>% 
  dplyr::arrange(-mean_Value) %>% 
  select(Country, 
         mean_Doctors_and_Graduates_per_1000,
         mean_Value) %>% 
  filter(mean_Value != "NA") %>% 
  filter(mean_Doctors_and_Graduates_per_1000 != "NA") %>% 
  select(c(1,2,3)) %>% 
  knitr::kable(caption = "Hip Replacements waiting time against total doctors and medical graduates in 2020", digits = c(0, 1, 0),
               col.names = c("Country",
                             "Doctors & Medical Graduates per 1000",
                             "Waiting times for Hip Procedures Mean days")) %>%
  kable_classic_2()

stat_analysis <- joinedfortablehip22 %>% 
  dplyr::arrange(-mean_Value) %>% 
  select(Country, 
         mean_Doctors_and_Graduates_per_1000,
         mean_Value) %>% 
  filter(mean_Value != "NA") %>% 
  filter(mean_Doctors_and_Graduates_per_1000 != "NA")

cor.test(stat_analysis$mean_Doctors_and_Graduates_per_1000, stat_analysis$mean_Value)









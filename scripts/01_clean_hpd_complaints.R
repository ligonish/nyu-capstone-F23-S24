# Capstone Team 4: Cleaning & Exploration
# 2016 - 2023 NYC Rent-Impairing Class C Violations
# 2023-11-11
# SL

# Libraries
library(tidyverse) # rapid data manipulation
library(janitor)   # cleaner variable names & simple tables
library(lubridate) # easier dates

# Data Import
ri_violations <- read_csv("data_raw/rent_impairing_class_c_violations_2012_to_2023.csv") %>% 
  remove_constant() %>% 
  select(!c(communityboard, councildistrict, originalcertifybydate, originalcorrectbydate, (certifieddate:newcorrectbydate))) %>% 
  mutate(inspectiondate = date(inspectiondate),
         approveddate = date(approveddate),
         novissueddate = date(novissueddate),
         currentstatusdate = date(currentstatusdate),
         inspection_yr = year(inspectiondate)) # just for initial peek; full analysis req's monthly

# Quick Summary Stats

ri_violations %>% 
  group_by(inspection_yr) %>% 
  summarize(tot_violations = n_distinct(violationid, na.rm = T)) %>% 
  ggplot(aes(x = inspection_yr,
             y = tot_violations)) +
  geom_col() +
  theme_minimal()

# Leaving off Saturday evening; next up is better summary stats of all
# class B/C, not just rent-impairing violations, which I'll do via
# aggregates inside the NYC OpenData hosted tables after some filtering.
# This should give us a sense of some directions worth exploring in 
# greater depth.







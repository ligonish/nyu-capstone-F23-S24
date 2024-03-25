# -----------------------------------------------------------------------------
# Capstone Team 4: Merge & Clean Geocoded Tract-Level Data into Analytic Set
# 2024-03-21
# SL
# -----------------------------------------------------------------------------

options(scipen=999) # removes scientific notation in GEOID field etc

# Dependent Packages ----------------------------------------------------------

library(tidyverse)     # cleaner data manipulation
library(janitor)       # cleaner variable names
library(campfin)       # normalize address spellings


# Load data

geocoded_vio <- read_csv("data_build/geocoded_violation_addresses.csv") %>%   
  mutate(geoid = glue("36{county_fips}{census_tract_20}"))

rtc_zips <- read_csv("data_build/rtc_zip_rollout.csv") %>% # Source: Ellen et al (2021), "Early Evidence on Eviction Patterns", pg.8 footnotes
  clean_names() %>% 
  mutate(zip = as.numeric(zip)) %>% 
  rename(rtc_treat_date = date_added) %>% 
  select(cohort, rtc_treat_date, zip)

tract_zip_assignments <- read_csv("data_build/tb_tract_zip_assignments.csv") %>% 
  clean_names() %>% 
  filter(res_ratio > 0) %>%   # SL added in March, since several tracts had no residential addresses in a given zip
  select(zip, tract) %>% 
  rename (geoid = tract) %>% 
  left_join(rtc_zips, by = "zip") %>%  
  mutate(
    cohort = replace_na(cohort, 5),
    rtc_treat_date = replace_na(rtc_treat_date, as_date("2021-05-11")),
    geoid = as.character(geoid)
    ) %>% 
  rename(tract_treat_zip = zip)

geocoded_ev <- read_csv("data_build/geocoded_evictions.csv") %>% 
  rename(geoid = geoid_tract_20) %>% 
  select(address, geoid)

geocoded_rs <- read_csv("data_build/rs17_tract_counts.csv") %>% 
  clean_names() %>% 
  mutate(geoid = as.character(geoid))

acs <- read_csv("data_build/acs5_tracts_12_23_renter_occ_units.csv") %>% 
  clean_names() %>% 
  select(!name) %>% 
  mutate(geoid = as.character(geoid))


# Generate tract-level eviction & violation counts 

evictions <- read_csv("data_raw/evictions_2017.csv") %>% 
  mutate(clean_address = normal_address(eviction_address)) %>% 
  mutate(address = paste(clean_address, eviction_zip, sep = ", NEW YORK, NY ")) %>% 
  left_join(geocoded_ev, by = "address") %>% 
  group_by(geoid) %>% 
  summarize(evictions_per_tract = n_distinct(docket_number)) # 763 evictions weren't properly geocoded

#n_occur <- data.frame(table(evictions$address)) # note some addresses had as many as 15 evictions in 2017

# Generate tract-level violation counts 

analyze <- read_csv("data_build/cleaner_utility_violations.csv.gz") %>%
  mutate(year = year(inspection_yr_mo)) %>%  
  mutate(address = paste(housenumber, streetname), .after = zip) %>% 
  mutate(address = paste(address, zip, sep = ", NEW YORK, NY ")) %>%    
  left_join(geocoded_vio, by = "address") %>%   # adds geoid to each violation
#  left_join(tract_zip_assignments, by = "geoid") %>%    # adds treatment date to each violation
  group_by(geoid, inspection_yr_mo) %>% 
  summarize(n_violations = n_distinct(violationid, na.rm = F)) %>%     # 17,640 obs from Jan 2012 - Feb 2020 
  ungroup() %>% 
  complete(geoid, inspection_yr_mo, fill = list(n_violations = 0)) # expand all 12 annual yearmonth observations for all zip codes, even if no violations reported


# Merge tract-level covariates for analysis

analyze <- analyze %>% 
  mutate(year = year(inspection_yr_mo)) %>% 
  group_by(geoid, inspection_yr_mo) %>% 
  left_join(acs, by = c("year", "geoid")) %>%   # Census covariates
  left_join(evictions, by = "geoid") %>% 
  left_join(geocoded_rs, by = "geoid") %>% 
  mutate(n_violations_per_1k_units = ((n_violations/renter_occ_units)*1000), .after = n_violations) %>% 
  ungroup() %>% 
  group_by(geoid) %>% 
  mutate(
    evict_rate_17 = case_when(
      year == 2017 ~ (evictions_per_tract/renter_occ_units)),   # eviction rate
    rs_rate_17 = case_when(
      year == 2017 ~ (rs_17_count/renter_occ_units)),   # rent stabilization rate
    pct_pov_17 = case_when(
      year == 2017 ~ pct_pov)   # poverty rate
  ) %>% 
  fill(evict_rate_17, rs_rate_17, pct_pov_17, .direction = "updown") %>% 
  select(-year) %>% 
  ungroup()

# 300,384 observations, albeit with a lot of missing data.

# Generate RTC Treatment Status Indicator

analyze <- analyze %>% 
  left_join(tract_zip_assignments, by = "geoid")

# Save

write_csv(analyze, "data_build/tract_set_for_analysis.csv.gz")







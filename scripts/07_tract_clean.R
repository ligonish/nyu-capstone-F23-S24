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
#  distinct(violationid, .keep_all = T) %>% 
  select(address, geoid)

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
  select(address, geoid) %>% 
  #distinct(address, .keep_all = T) %>% 
  mutate(geoid = as.character(geoid))

geocoded_rs <- read_csv("data_build/rs17_tract_counts.csv") %>% 
  clean_names() %>% 
  mutate(geoid = as.character(geoid))

acs <- read_csv("data_build/acs5_tracts_12_23_renter_occ_units.csv") %>% 
  clean_names() %>% 
  select(!name) %>% 
  mutate(geoid = as.character(geoid))

acs_missing <- acs %>% 
  filter(if_any(everything(), is.na)) # 1,426 of 19,663 contain missing obs


# Generate tract-level eviction counts 

evictions <- read_csv("data_raw/evictions_2017.csv") %>% 
  mutate(address = normal_address(eviction_address)) %>% 
  mutate(
    address = str_replace_all(address, "STREE T|S TREET|STRE ET|ST REET|STR EET|STREE(?=[:punct:])",
                              "ST"),
    address = str_replace_all(address, "A VENUE|AV ENUE|AVE NUE|AVEN UE|AVENU E|AV ENUE|AV |AVE ",
                              "AVE"),
    address = str_replace_all(address, "P ARKWAY|PA RKWAY|PAR KWAY|PARKW AY|PKWY",
                              "PKWY"),
    address = str_replace_all(address, "CONC OURSE",
                              "CONCOURSE")) %>% 
  left_join(geocoded_ev, by = "address") %>%     # 13,601 obs of 10 variables
  group_by(geoid) %>% 
  summarize(evictions_per_tract = n_distinct(docket_number)) # 1,902 obs of 2 variables

# n_occur <- data.frame(table(evictions$address)) # note some addresses had as many as 15 evictions in 2017

# Generate tract-level violation counts 

analyze <- read_csv("data_build/cleaner_utility_violations.csv.gz") %>%
  mutate(year = year(inspection_yr_mo)) %>%  
  mutate(address = paste(housenumber, streetname), .after = zip) %>% 
  mutate(
    address = str_replace_all(address, "HOR HARDING EXPRESSWAY SR NORTH",
                              "HORACE HARDING EXPY"),
    address = str_replace_all(address, "HOR HARDING EXPRESSWAY SR SOUTH",
                              "HORACE HARDING EXPY"),
    address = str_replace_all(address, "ST NICHOLAS PLACE",
                              "SAINT NICHOLAS PL"),
    address = str_replace_all(address, "(?<=[:space:])9 AVENUE",
                              "9TH AVENUE"),
    address = str_replace_all(address, "WEST 181 STREET",
                              "W 181ST ST"),
    address = str_replace_all(address, "FREDERICK DOUGAL",
                              "FREDERICK DOUGLASS"),
    address = str_replace_all(address, "MANH(?=[:space:])",
                              "MANHATTAN"),
    address = str_replace_all(address, "EAST 6(?=[:space:])",
                              "EAST 6TH"),
    address = str_replace_all(address, "EAST 6(?=[:space:])",
                              "EAST 6TH"),
    address = str_replace_all(address, "EAST 12(?=[:space:])",
                              "EAST 12TH"),
    address = str_replace_all(address, "EAST 13(?=[:space:])",
                              "EAST 13TH"),
    address = str_replace_all(address, "EAST 14(?=[:space:])",
                              "EAST 14TH"),
    address = str_replace_all(address, "EAST 15(?=[:space:])",
                              "EAST 15TH"),
    address = str_replace_all(address, "EAST 31(?=[:space:])",
                              "EAST 31ST"),
    address = str_replace_all(address, "EAST 39(?=[:space:])",
                              "EAST 39TH"),
    address = str_replace_all(address, "EAST 57(?=[:space:])",
                              "EAST 57TH"),
    address = str_replace_all(address, "EAST 132(?=[:space:])",
                              "EAST 132ND"),
    address = str_replace_all(address, "EAST 174(?=[:space:])",
                              "EAST 174TH"),
    address = str_replace_all(address, "EAST 176(?=[:space:])",
                              "EAST 176TH"),
    address = str_replace_all(address, "EAST 187(?=[:space:])",
                              "EAST 187TH"),
    address = str_replace_all(address, "EAST 238(?=[:space:])",
                              "EAST 238TH"),
    address = str_replace_all(address, "MC BRIDE",
                              "MCBRIDE"),
    address = str_replace_all(address, "ADAM C POWELL BOULEVARD",
                              "ADAM CLAYTON POWELL JR BLVD"),
    address = str_replace_all(address, "ADAM C POWELL JR BOULEVARD",
                              "ADAM CLAYTON POWELL JR BLVD"),
    address = str_replace_all(address, "HUTCH RIVER",
                              "HUTCHINSON RIVER"),
    address = str_replace_all(address, "AVENUE",
                              "AVE"),
    address = str_replace_all(address, "STREET",
                              "ST"),
    address = str_replace_all(address, "PARKWAY",
                              "PKWY"),
    address = str_replace_all(address, "BOULEVARD",
                              "BLVD")) %>% 
  left_join(geocoded_vio, by = "address") %>%     # adds geoid to each violation
#  left_join(tract_zip_assignments, by = "geoid") %>%    # adds treatment date to each violation
 group_by(geoid, inspection_yr_mo) %>% 
  summarize(n_violations = n_distinct(violationid, na.rm = F)) %>%
  ungroup() %>% 
  complete(geoid, inspection_yr_mo, fill = list(n_violations = 0)) # expand all 12 annual yearmonth observations for all zip codes, even if no violations reported


# Merge tract-level covariates for analysis

analyze <- analyze %>% 
  mutate(year = year(inspection_yr_mo),
         geoid = as.character(geoid)) %>% 
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
      year == 2017 ~ pct_pov),   # poverty rate
    pct_wh_17 = case_when(
      year == 2017 ~ pct_wh_rou)   # time-invariant control for baseline POC residents at treatment selection year
  ) %>% 
  fill(evict_rate_17, rs_rate_17, pct_pov_17, pct_wh_17, .direction = "updown") %>% 
  select(-year) %>% 
  ungroup()

# Generate RTC Treatment Status Indicator

analyze <- analyze %>% 
  left_join(tract_zip_assignments, by = "geoid")

# Save

write_csv(analyze, "data_build/tract_set_for_analysis.csv.gz")







# -----------------------------------------------------------------------------
# Capstone Team 4: Cleaning & Exploration
# 2012 - 2023 Utility-Related Violations
# 2023-12-08
# SL
# -----------------------------------------------------------------------------


# Dependent Packages ----------------------------------------------------------

library(tidyverse) # data manipulation
library(janitor)   # cleaner variable names
library(lubridate) # normalizes date objects

# Data Import -----------------------------------------------------------------

# Continue from scripts/00_get_violations and 01/get_census, or:

violations <- read_csv("data_raw/utility_violations_2012_onward.csv.gz") %>% 
  remove_constant() %>% 
  select(violationid, # these can shift with our analytic needs; just shortening for processing time now
         buildingid,
         boroid,
         boro,
         housenumber,
         lowhousenumber,
         highhousenumber,
         streetname,
         zip, 
         apartment,
         story,
         block,
         lot,
         class,
         inspectiondate,
         ordernumber,
         novdescription,
         novtype,
         rentimpairing,
         latitude,
         longitude,
         censustract,
         bin,
         bbl,
         nta) %>% 
  mutate(inspectiondate = date(inspectiondate),  # baseline date for each observation; earlier date would require using complaints database instead/as well
         inspection_yr = year(inspectiondate),  # for manipulation/visualization/interactions  
         inspection_mo = month(inspectiondate), # allows analysis of seasonal trends and overall more detailed time-constant fixed effects (esp important in staggered DiD)
         inspection_yr_mo = floor_date(date(inspectiondate), "month")) # we can just concatenate these or use the zoo package for a YYYY_MM format if that winds up being an FE or other explanatory var; this is just for sketching shape of data


# Add Year/Month Level RTC Treatment Dates & Indicators ------------------------

rtc_zips <- read_csv("data_build/rtc_zip_rollout.csv") %>% # Source: Ellen et al (2021), "Early Evidence on Eviction Patterns", pg.8 footnotes
  clean_names() %>% 
  mutate(zip = as.numeric(zip)) %>% 
  rename(rtc_treat_date = date_added) %>% 
  select(cohort, rtc_treat_date, zip) %>%
  filter(cohort != 5)

violations <- violations %>% 
  left_join(rtc_zips, by = "zip") %>% 
  mutate(
    cohort = replace_na(cohort, 5),
    rtc_treat_date = replace_na(rtc_treat_date, as_date("2021-05-11")), # all ZIPS not in cohorts 1-4 were added to treatment May 2021
    treated = case_when(
      rtc_treat_date > inspectiondate ~ 0,
      TRUE ~ 1)  # indicates whether violation inspection was on unit with RTC
  )  

# Check for missing ZIP codes --------------------------------------------------

zip_summary <- violations %>% 
  group_by(zip) %>% 
  summarize(n_violations = n_distinct(violationid))%>% 
  arrange(desc(zip)) 

# 183 distinct ZIP codes in violation data, 2012-2023
# 188 violation observations with faulty zip entries (impute these from PLUTO using bbl and/or street address):
     # 178 with NA zips
     # 5 with zip entered as "2016" 
     # 1 with zip entered as "2015" 
     # 2 with "112226" when street address indicates they meant "11226" (delete extra 2)
     # 2 with "111010" when street address indicates they meant "11101" (delete final 0)

faulty_zips <- violations %>% 
  filter(zip == "2015"| zip == "2016" | zip == "112226" | zip == "111010" | is.na(zip)) %>%   # 188 clearly faulty zip values total, Jan 2012-Feb 2020
  select(violationid, bbl, boroid, boro, housenumber, streetname, zip, censustract, inspectiondate) %>% 
  arrange(inspectiondate)  # 79 fall in our pre-COVID timeline of interest

write_csv(faulty_zips, "data_build/missing_or_faulty_zips_2012_2020.csv")

# Save 2012-2023 Violations ----------------------------------------------------

write_csv(violations, "data_build/cleaner_utility_violations.csv.gz")

# Filter Pre-COVID ZIP-Level Data for Initial Descriptive Stats -------------

corrected_zips <- read.csv("data_build/corrected_zips_2012_2020.csv") %>%  # hand corrected by Jiayi/Reba
  mutate(inspectiondate = date(inspectiondate),  # keeps date syntax compatible with rest of data after merge
         inspection_yr = year(inspectiondate),  
         inspection_mo = month(inspectiondate), 
         inspection_yr_mo = floor_date(date(inspectiondate), "month")) 

precovid_zip_violations <- violations %>% 
  filter(inspectiondate <= "2020-02-29",
         zip != "2016",
         !is.na(zip)) %>%
  bind_rows(corrected_zips) %>%   # updated from rbind.fill to dplyr syntax so rest of code runs in tidyverse
  mutate(zip = case_when( # correcting a few further misentered ZIPs discovered during later analysis
    zip == 10129 ~ 10128, # violation ID 11725954 entered with ZIP typo
    zip == 10435 ~ 11435, # violation ID 10549312 entered with ZIP typo
    zip == 11452 ~ 10452, # violation IDs 10599781 & 10599866 entered with ZIP typos
    TRUE ~ zip
  ))

for_analysis <- precovid_zip_violations %>% 
  group_by(zip, inspection_yr_mo) %>% 
  summarize(n_violations = n_distinct(violationid, na.rm = F)) %>%     # 17,640 obs from Jan 2012 - Feb 2020 
  ungroup() %>% 
  complete(zip, inspection_yr_mo, fill = list(n_violations = 0)) # expand all 12 annual yearmonth observations for all zip codes, even if no violations reported

# Import ZIP-Level Eviction Counts, Rent Stabilized Unit Counts, and ACS 5-Yr Estimates ------------------------

acs_zcta <- read_csv("data_build/acs5_zctas_12_20_renter_occ_units.csv") %>% 
  rename(zip = GEOID)

evict <- read_csv("data_raw/jan_to_aug_evictions_2017.csv") %>% # 2017 baseline Jan-Aug eviction counts, by DOI and collated in NYC OpenData
  clean_names %>% 
  mutate(zip = as.numeric(eviction_postcode), .keep = "unused") %>% 
  rename(evicts_17 = count)

rs <- read_csv("data_build/rs_17_full.csv") %>%   # baseline 2017 rent-stabilized unit counts, via Furman + Tony
  mutate(zip = as.numeric(zipcode), .keep = "unused") %>% 
  rename(rs_17 = rs_17_count)

# Merge/Generate Census, Eviction, & Rent Stabilization Covariates -------------------------------------------------------------

for_analysis <- for_analysis %>% 
  mutate(year = year(inspection_yr_mo)) %>%  
  group_by(zip, inspection_yr_mo) %>% 
  left_join(acs_zcta, by = c("year", "zip")) %>%   # Census covariates
  left_join(evict, by = "zip") %>%    # eviction covariate
  left_join(rs, by = "zip") %>%   # rent stabilization covariate 
  mutate(n_violations_per_1k_units = ((n_violations/renter_occ_units)*1000), .after = n_violations) %>% 
  ungroup() %>% 
  group_by(zip) %>% 
  mutate(
    evict_rate_17 = case_when(
      year == 2017 ~ (evicts_17/renter_occ_units)),   # eviction rate
    rs_rate_17 = case_when(
      year == 2017 ~ (rs_17/renter_occ_units)),   # rent stabilization rate
    pct_pov_17 = case_when(
      year == 2017 ~ pct_pov)   # poverty rate
  ) %>% 
  fill(evict_rate_17, rs_rate_17, pct_pov_17, .direction = "updown") %>% 
  select(-year) %>% 
  ungroup()

# Generate UA Treatment Status Indicator ----------------------------------------

for_analysis <- for_analysis %>% 
  group_by(zip, inspection_yr_mo) %>% 
  left_join(rtc_zips, by = "zip") %>% 
  mutate(
    cohort = replace_na(cohort, 5),
    rtc_treat_date = replace_na(rtc_treat_date, as_date("2021-05-11")), # all ZIPS not in cohorts 1-4 were added to treatment May 2021
    treated = case_when(
      rtc_treat_date > inspection_yr_mo ~ 0,
      TRUE ~ 1)) %>% 
  ungroup



# Save -------------------------------------------------------------------------

write_csv(for_analysis, "data_build/2012_2020_zip_set_for_analysis.csv.gz")


# Add Tract-Level ACS 5-Yr Occupied Renter Unit Estimates ----------------------

# For "violation intensity" outcome, as n monthly violations per 1k renter-occupied units in tract

# NB this won't merge well or be accurate until violation set's census tract values
# are more thoroughly checked, cleaned, & crosswalked; adding here for initial code sketch)

# tract_units <-  read_csv("data_build/acs5_tracts_12_23_renter_occ_units.csv") 
############## continue here after crosswalking


# FOR TEAM: Next-Up Cleaning Priorities ----------------------------------------

     # Impute the 79 missing ZIP code values (Jan 2012-Feb 2020) via PLUTO database and street address
     # If necessary, wrangle census tract value format to allow merging census estimates into violation set
     # If necessary, merge in tract-level census estimates
     # Make sure we wind up with a violation intensity rate for ALL residential zips/tracts, since some may not be included in violation data
     # [etc.!]


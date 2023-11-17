# -----------------------------------------------------------------------------
# Capstone Team 4: Cleaning & Exploration
# 2012 - 2023 Utility-Related Violations
# 2023-11-14
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

rm(rtc_zips)


# Save -------------------------------------------------------------------------

write_csv(violations, "data_build/cleaner_utility_violations.csv.gz")


# Add ZIP-Level ACS 5-Yr Occupied Renter Unit Estimates ------------------------

# For "violation intensity" visualization, as n violations per 1k renter-occupied units in ZIP.
# We'll use tract-level version of this as outcome of interest; see below.

# NB this won't merge well or be accurate until violation set's ZIP values
# are more thoroughly checked, cleaned, & crosswalked. We'll also need to crosswalk 
# the ZCTAs to their 2010 and 2020-era ZIP codes for merge, since ZCTAs and ZIPs 
# aren't identical. Below is just a rough sketch for early data peek.)

zcta_units <-  read_csv("data_build/acs5_zctas_12_23_renter_occ_units.csv") %>% 
  rename(inspection_yr = year) %>% 
  select(inspection_yr, zip, renter_occ_units)

zip_intensity <- violations %>% 
  left_join(zcta_units, by = c("inspection_yr", "zip")) %>%
  group_by(zip, inspection_yr) %>% 
  mutate(n_annual_zip_violations = n_distinct(violationid)) %>% 
  mutate(yr_intensity = (n_annual_zip_violations/renter_occ_units)*1000) 


# Save -------------------------------------------------------------------------

write_csv(violations, "data_build/rough_annual_zip_violation_intensities.csv.gz")


# Add Tract-Level ACS 5-Yr Occupied Renter Unit Estimates ----------------------

# For "violation intensity" outcome, as n monthly violations per 1k renter-occupied units in tract

# NB this won't merge well or be accurate until violation set's census tract values
# are more thoroughly checked, cleaned, & crosswalked; adding here for initial code sketch)

# tract_units <-  read_csv("data_build/acs5_tracts_12_23_renter_occ_units.csv") 
############## continue here after crosswalking


# FOR TEAM: Next-Up Cleaning Priorities ----------------------------------------

     # Thorough ID of missing values
     # Summary analysis to turn up misrecorded ZIP codes and/or census tract numbers
     # Crosswalk ZCTA to 2010 and 2020 NYC ZIP code
     # Wrangle census tract value format to allow merging census estimates into violation set
     # Merge in tract-level census estimates
     # Make sure we wind up with a violation intensity rate for ALL residential zips/tracts, since some may not be included in violation data
     # [etc.!]


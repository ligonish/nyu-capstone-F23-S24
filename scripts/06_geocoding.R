# -----------------------------------------------------------------------------
# Capstone Team 4: Geocoding Violation & Eviction Data by Census Tract
# 2024-03-11
# SL
# -----------------------------------------------------------------------------

# Dependent Packages ----------------------------------------------------------

library(tidyverse)  # cleaner data manipulation
library(RSocrata)   # pulls from NYC OpenData API
library(janitor)    # cleaner variable names
library(glue)       # easier than paste0
library(tidygeocoder)  # geocodes by connecting to US Census Bureau API, Geocodio, OSM, and a couple other options

# Load 2010 <--> 2020 Census tract relationship files --------------------------

tracts_20_10 <- read.table("data_raw/census_ny_2020_to_2010_tract_relationships.txt",sep = "|", header = T) %>% 
  clean_names() %>% 
  select(geoid_tract_20, geoid_tract_10) %>% 
  mutate(census_tract_10 = str_sub(geoid_tract_10, -6, -1),
         census_tract_20 = str_sub(geoid_tract_20, -6, -1))

# Generate full street addresses for Housing Maintenance Violations data -------

raw_lat_long <- read_csv("data_raw/utility_violations_2012_onward.csv.gz") %>% 
  select(violationid, housenumber, streetname, zip, latitude, longitude) %>% 
  mutate(address = paste(housenumber, streetname), .after = zip) %>% 
  mutate(address = paste(address, zip, sep = ", NEW YORK, NY "))   #104,050 obs of 7 vars

write_csv(raw_lat_long, "data_build/violation_ids_lats_longs_12_23.csv")


# Divide Housing Maintenance Violations data into three 10k-obs chunks ---------

# NB: Census geocoder will only accept 10,000 rows at a time, and this is a 
# memory-hungry process generally. Here, I pull only the distinct building street 
# addresses from all 104,050 unique violations, then split that set into 

addresses_01 <- raw_lat_long %>%
  distinct(address) %>%    #28,924 obs
  slice(1:10000)   # 10,000 rows

addresses_02 <- raw_lat_long %>%
  distinct(address) %>%    
  slice(10001:20000) #10,000 rows

addresses_03 <- raw_lat_long %>%
  distinct(address) %>%    
  slice(20001:28924) #8,924 rows

# Geocode Housing Maintenance Violations data ----------------------------------

# Batch 1/3

geocoded_addr_01 <- addresses_01 %>% 
  geocode(address = address,
          lat = 'latitude',
          long = 'longitude',
          method = "census",
          full_results = TRUE,
          api_options = list(census_return_type = "geographies")) %>% 
  rename(census_tract_20 = census_tract) %>% 
  select(address, county_fips, census_tract_20)

# Batch 2/3

geocoded_addr_02 <- addresses_02 %>% 
  geocode(address = address,
          lat = 'latitude',
          long = 'longitude',
          method = "census",
          full_results = TRUE,
          api_options = list(census_return_type = "geographies")) %>% 
  rename(census_tract_20 = census_tract) %>% 
  select(address, county_fips, census_tract_20)

# Batch 3/3

geocoded_addr_03 <- addresses_03 %>% 
  geocode(address = address,
          lat = 'latitude',
          long = 'longitude',
          method = "census",
          full_results = TRUE,
          api_options = list(census_return_type = "geographies")) %>% 
  rename(census_tract_20 = census_tract) %>% 
  select(address, county_fips, census_tract_20)

# Stick all three geocoded violations addresses back together!

geocoded <- geocoded_addr_01 %>% 
  bind_rows(list(geocoded_addr_02, geocoded_addr_03))


# Add a full-length GEOID (c. 2020 tract numeration version)

geocoded <- geocoded %>% 
  mutate(geoid_tract_20 = glue("36{county_fips}{census_tract_20}"))


# Save! ------------------------------------------------------------------------

write_csv(geocoded, "data_build/geocoded_violation_addresses.csv")


# Assign "treatment" at the tract level using Tony's code ----------------------

rtc_zips <- read_csv("data_build/rtc_zip_rollout.csv") %>% # Source: Ellen et al (2021), "Early Evidence on Eviction Patterns", pg.8 footnotes
  clean_names() %>% 
  mutate(zip = as.numeric(zip)) %>% 
  rename(rtc_treat_date = date_added) %>% 
  select(cohort, rtc_treat_date, zip) %>%
  filter(cohort != 4 & cohort != 5) %>%
  mutate(treated = 1) %>%
  select(zip, treated)

zip_tract <- read_xlsx("data_raw/HUD_tract_to_zip_crosswalk_2012_q4.xlsx", col_types = "numeric") %>%
  filter(TRACT >= "36000000000" & TRACT < "37000000000") %>%
  mutate(state_code = substr(TRACT, start = 1, stop = 2),
         county_code = substr(TRACT, start = 3, stop = 5),
         tract_code = substr(TRACT, start = 6, stop = 11)) %>%
  filter(county_code %in% c("005", "047", "061", "081", "085")) %>%
  left_join(rtc_zips, by = c("ZIP" = "zip")) %>%
  replace(is.na(.), 0) %>%
  group_by(TRACT) %>%
  mutate(
    IMPACTED = n_distinct(treated) > 1,
    ROWNUM = row_number(),
    MAX_RES = max(RES_RATIO)
  ) %>%
  filter(RES_RATIO == MAX_RES) %>%
  select(TRACT, ZIP, RES_RATIO, IMPACTED)

# Save

write_csv(zip_tract, "data_build/tb_tract_zip_assignments.csv")





# Get 2017 eviction data -------------------------------------------------------

token <- "7364W6XxzbdfnyrT35ZCO40Vs"   # your local API token for NYC OpenData goes here

ev_url <- "https://data.cityofnewyork.us/resource/6z8x-wfk4.json?$where= executed_date >= '2017-01-01T00:00:00.000' and executed_date <= '2017-07-31T00:00:00.000'"

evictions <- read.socrata(url = ev_url, 
                          app_token = token) %>% 
  clean_names() %>% 
  select(docket_number, executed_date, borough, census_tract)   # 13,601 obs, 4 vars


# Collapse to eviction totals by tract -----------------------------------------

evictions <- evictions %>% 
  group_by(borough, census_tract) %>%    # including both since some tract numbers are repeated in different boroughs
  summarize(n_evictions = n_distinct(docket_number)) %>% 
  mutate(county_fips = case_when(
    borough == 'BROOKLYN' ~ '047',
    borough == 'BRONX' ~ '005',
    borough == 'MANHATTAN' ~ '061',
    borough == 'QUEENS' ~ '081',
    borough == 'STATEN ISLAND' ~ '085'
  ), .before = census_tract)   # 1,765 of 4 obs



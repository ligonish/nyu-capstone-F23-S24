# -----------------------------------------------------------------------------
# Capstone Team 4: Geocoding Violation & Eviction Data by Census Tract
# 2024-03-11
# SL
# -----------------------------------------------------------------------------

# Dependent Packages ----------------------------------------------------------

library(tidyverse)     # cleaner data manipulation
library(RSocrata)      # pulls from NYC OpenData API
library(readxl)        # reads Excel files
library(janitor)       # cleaner variable names
library(glue)          # easier than paste0
library(campfin)       # normalize address spellings
library(tidygeocoder)  # geocodes by connecting to US Census Bureau API, Geocodio, OSM, and a couple other options

# Load 2010 <--> 2020 Census tract relationship files --------------------------

tracts_20_10 <- read.table("data_raw/census_ny_2020_to_2010_tract_relationships.txt",sep = "|", header = T) %>% 
  clean_names() %>% 
  select(geoid_tract_20, geoid_tract_10) %>% 
  mutate(census_tract_10 = str_sub(geoid_tract_10, -6, -1),
         census_tract_20 = str_sub(geoid_tract_20, -6, -1))

# GEOCODE VIOLATIONS -----------------------------------------------------------
# Generate full street addresses for Housing Maintenance Violations data -------

raw_lat_long <- read_csv("data_raw/utility_violations_2012_onward.csv.gz") %>%    
  select(violationid, boroid, censustract, housenumber, streetname, zip, latitude, longitude) %>% 
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
  mutate(state = "NY")

write_csv(raw_lat_long, "data_build/violation_ids_lats_longs_12_23.csv")


# Divide Housing Maintenance Violations data into three 10k-obs chunks ---------

# NB: Census geocoder will only accept 10,000 rows at a time, and this is a 
# memory-hungry process generally. Here, I pull only the distinct building street 
# addresses from all 104,050 unique violations, then split that set into 

addresses_01 <- raw_lat_long %>%
  distinct(address, .keep_all = T) %>%    #28,924 obs
  slice(1:10000)   # 10,000 rows

addresses_02 <- raw_lat_long %>%
  distinct(address, .keep_all = T) %>%    
  slice(10001:20000) #10,000 rows

addresses_03 <- raw_lat_long %>%
  distinct(address, .keep_all = T) %>%    
  slice(20001:28924) #8,924 rows

# Geocode Housing Maintenance Violations data ----------------------------------

# Batch 1/3

geocoded_addr_01 <- addresses_01 %>% 
  geocode(street = 'address',
          state = 'state',
          postalcode = 'zip',
          lat = 'latitude',
          long = 'longitude',
          method = "census",
          full_results = TRUE,
          custom_query = list(benchmark = "Public_AR_Census2020", 
                              vintage = "Census2010_Census2020"),  # 2010 Census geographies (for accurate merging w/ ACS set)
          api_options = list(census_return_type = "geographies")) %>% 
  select(violationid, boroid, address, zip, county_fips, censustract, census_tract)

# Batch 2/3

geocoded_addr_02 <- addresses_02 %>% 
  geocode(street = 'address',
          state = 'state',
          postalcode = 'zip',
          lat = 'latitude',
          long = 'longitude',
          method = "census",
          full_results = TRUE,
          custom_query = list(benchmark = "Public_AR_Census2020", 
                              vintage = "Census2010_Census2020"),   # 2010 Census geographies (for accurate merging w/ ACS set)
          api_options = list(census_return_type = "geographies")) %>% 
  select(violationid, boroid, address, zip, county_fips, censustract, census_tract)

# Batch 3/3

geocoded_addr_03 <- addresses_03 %>% 
  geocode(street = 'address',
          state = 'state',
          postalcode = 'zip',
          lat = 'latitude',
          long = 'longitude',
          method = "census",
          full_results = TRUE,
          custom_query = list(benchmark = "Public_AR_Census2020", 
                              vintage = "Census2010_Census2020"),   # 2010 Census geographies (for accurate merging w/ ACS set)
          api_options = list(census_return_type = "geographies")) %>% 
  select(violationid, boroid, address, zip, county_fips, censustract, census_tract)

# Stick all three geocoded violations addresses back together!

geocoded <- geocoded_addr_01 %>% 
  bind_rows(list(geocoded_addr_02, geocoded_addr_03))

# Add a full-length GEOID (c. 2020 tract numeration version)

geocoded <- geocoded %>% 
  mutate(geoid = glue("36{county_fips}{census_tract}"),
         geoid = na_if(geoid, "36NANA")) 

rm(addresses_01, addresses_02, addresses_03, geocoded_addr_01, geocoded_addr_02, geocoded_addr_03)

# Check for ungeocoded violation rows ------------------------------------------

uncoded_violations <- geocoded %>% 
  filter(is.na(geoid)) %>% 
  select(-county_fips)   # 197 addresses weren't geocoded (of 28,903 distinct addresses, so less than 1%)

# Read in NYC Planning's tract key to help fill in blanks (note: Planning uses decimals in a way inconsistent w this set's tract numeration, so not all will merge)

tract_key <- read_excel("data_raw/nyc_2020_census_tract_nta_cdta_relationships.xlsx") %>% 
  clean_names() %>% 
  select(geoid, boro_code, ct_label) %>% 
  rename(censustract = ct_label,
         boroid = boro_code,
         geoid_nyp = geoid)

geocoded <- geocoded %>% 
  mutate(geoid = as.character(geoid)) %>%
  mutate(censustract = as.character(censustract)) %>% 
  left_join(tract_key, by = c('boroid', 'censustract')) %>%  
  mutate(geoid = case_when(
    is.na(geoid) ~ geoid_nyp,
    TRUE ~ geoid)
  ) %>% 
  mutate(geoid = na_if(geoid, "36NANA")) %>% 
  select(-geoid_nyp)

uncoded_violations <- geocoded_test %>% 
  filter(is.na(geoid))  # down to 70 from 197

# Save! ------------------------------------------------------------------------

write_csv(geocoded, "data_build/geocoded_violation_addresses.csv")
write_csv(uncoded_violations, "data_build/uncoded_violations.csv")

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


# GEOCODE EVICTIONS ------------------------------------------------------------

# Get 2017 eviction data -------------------------------------------------------

token <- "7364W6XxzbdfnyrT35ZCO40Vs"   # your local API token for NYC OpenData goes here

ev_url <- "https://data.cityofnewyork.us/resource/6z8x-wfk4.json?$where= executed_date >= '2017-01-01T00:00:00.000' and executed_date <= '2017-07-31T00:00:00.000'"

evictions <- read.socrata(url = ev_url, 
                          app_token = token) %>% 
  clean_names() %>% 
  select(docket_number, executed_date, borough, census_tract, eviction_address, eviction_zip, latitude, longitude)   # 13,601 obs, 8 vars

# Save

write_csv(evictions, "data_raw/evictions_2017.csv")

# Geocode eviction addresses ---------------------------------------------------

# Generate borid variable to merge with Planning Dept key

evictions <- read_csv("data_raw/evictions_2017.csv") %>% 
  mutate(boroid = case_when(
    borough == "MANHATTAN" ~ 1,
    borough == "BRONX" ~ 2,
    borough == "BROOKLYN" ~ 3,
    borough == "QUEENS" ~ 4,
    borough == "STATEN ISLAND" ~ 5
  ), .after = borough) %>% 
  rename(censustract = census_tract) %>% 
  mutate(state = "NY")

# Generate full street address

evictions <- evictions %>% 
  mutate(address = normal_address(eviction_address)) %>% 
  mutate(
    address = str_replace_all(address, "STREE T|S TREET|STRE ET|ST REET|STR EET|STREE(?=[:punct:])",
                              "ST"),
    address = str_replace_all(address, "A VENUE|AV ENUE|AVE NUE|AVEN UE|AVENU E|AV ENUE|AV |AVE ",
                              "AVE"),
    address = str_replace_all(address, "P ARKWAY|PA RKWAY|PAR KWAY|PARKW AY|PKWY",
                              "PKWY"),
    address = str_replace_all(address, "CONC OURSE",
                              "CONCOURSE"))

# NB: all these addresses are messy af. we have entries like "STREE T", "ST.", "ST" for "STREET", as well as tons of added notes in the address field ("ENTIRE BASEMENT", "FRONT TWO BEDROOMS SHARING", etc.)

# Divide evictions data into two <= 10k-obs chunks -----------------------------

# NB: Census geocoder will only accept 10,000 rows at a time, and this is a 
# memory-hungry process generally. 

evictions_01 <- evictions %>%
  distinct(address, .keep_all = T) %>%    #10,476 obs
  slice(1:10000)   # 10,000 rows

evictions_02 <- evictions %>%
  distinct(address, .keep_all = T) %>%    
  slice(10001:10476) #476 rows

# Geocode evictions data -------------------------------------------------------

# Batch 1/2

geocoded_ev_01 <- evictions_01 %>% 
  geocode(street = 'address',
          state = 'state',
          postalcode = 'eviction_zip',
          lat = 'latitude',
          long = 'longitude',
          method = "census",
          full_results = TRUE,
          custom_query = list(benchmark = "Public_AR_Census2020", 
                              vintage = "Census2010_Census2020"),   # 2010 Census geographies (for accurate merging w/ ACS set)
          api_options = list(census_return_type = "geographies")) %>% 
  select(address, county_fips, boroid, censustract, census_tract)

# Batch 2/2

geocoded_ev_02 <- evictions_02 %>% 
  geocode(street = 'address',
          state = 'state',
          postalcode = 'eviction_zip',
          lat = 'latitude',
          long = 'longitude',
          method = "census",
          full_results = TRUE,
          custom_query = list(benchmark = "Public_AR_Census2020", 
                              vintage = "Census2010_Census2020"),
          api_options = list(census_return_type = "geographies")) %>%
  select(address, county_fips, boroid, censustract, census_tract)

# Stick both geocoded eviction sets back together!

geocoded_ev <- geocoded_ev_01 %>% 
  bind_rows(list(geocoded_ev_02)) #10,433 obs of 5 variables

# Add a full-length GEOID (c. 2020 tract numeration version)

geocoded_ev <- geocoded_ev %>% 
  mutate(geoid = glue("36{county_fips}{census_tract}")) %>% 
  mutate(geoid = na_if(geoid, "36NANA"))

# Isolate missing fields

uncoded_evictions <- geocoded_ev %>% 
  filter(is.na(geoid))  # 490 of 10,433 distinct addresses don't have a geoid

# Fill in some blanks with Planning Department's partially-helpful tract/GEOID crosswalk

geocoded_ev <- geocoded_ev %>% 
  mutate(geoid = as.character(geoid)) %>%
  mutate(censustract = as.character(censustract)) %>% 
  left_join(tract_key, by = c('boroid', 'censustract')) %>%  
  mutate(geoid = case_when(
    is.na(geoid) ~ geoid_nyp,
    TRUE ~ geoid)
  ) %>% 
  mutate(geoid = na_if(geoid, "36NANA"))

uncoded_evictions <- geocoded_ev %>% 
  filter(is.na(geoid))  # down to 198 uncoded, from 490 -- the rest need hand-edits due to messy/inconsistent data entry

# Save! ------------------------------------------------------------------------

write_csv(geocoded_ev, "data_build/geocoded_evictions.csv")
write_csv(uncoded_evictions, "data_build/uncoded_evictions.csv")

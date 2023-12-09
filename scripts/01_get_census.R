# -----------------------------------------------------------------------------
# Capstone Team 4: Adding ACS Occupied Rental Unit Count Estimates, by Census Tract
# 2012 - 2023 Utility-Related Violations
# 2023-12-08
# SL
# -----------------------------------------------------------------------------


# Dependent Packages ----------------------------------------------------------

library(readxl)   # import Census.gov ZIP->ZCTA crosswalks from .xls format
library(tidyverse)  # data manipulation
library(tidycensus) # allows direct download of ACS microdata
library(priceR)      # CPI-adjust dollar amounts


options(tigris_use_cache = TRUE) # runs faster
options(scipen=999) # removes scientific notation in GEOID field etc


# Census Data Pull ------------------------------------------------------------

# Start by getting a U.S. Census API key and storing it on your local system. 
# See https://walker-data.com/tidycensus/articles/spatial-data.html for tutorial.

# Here's an NYC-specific tidyverse guide https://justinmorganwilliams.medium.com/import-nyc-census-data-into-r-with-tidycensus-c94d2d1f23fa
# And one on getting multiple years of ACS data at once, using purrr loops https://mattherman.info/blog/tidycensus-mult-year/

# Tidycensus lets you browse all the available variables by year and census type:

v18 <- load_variables(2018, "acs5") # for example; acs 1-yr & decennial estimates also available

# Future work can use the below method to grab a larger vector of explanatory variables
# to do with shifting neighborhood demographics and housing characteristics.
# To help operationalize our outcome variable right away, though, I started with 
# B25032_013: Total renter-occupied housing units (universe: occupied housing units; 
# it's available down to block grp level. I used census tract to allow greater 
# statistical power than ZIP/ZCTA.)

# B25032_013 is available in the 5-year estimates through 2021, and the 1-yr through 
# 2022. I chose 5-yr for this dry run because 2020 provides *only* 5-yr estimates 
# (COVID chaos). We can discuss how to approach this. Potential solutions include 
# imputation, but I'd want to see how other researchers have navigated this issue 
# in recent publications -- the noncomparability of 2020 census estimates is at least
# a widely-shared problem right now.)

rm(v18) # browse away, then free up that memory!

years <- lst(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020) # for purrr looping later

# By ZCTA, 2012 - 2020 --------------------------------------------------------

# "ZCTA" maps very closely to ZIP code, but isn't identical because ZIP codes are 
# not technically proper spatial boundaries. We can crosswalk the former to the 
# latter as we keep working; that allows us to estimate violation rates at both 
# the tract and the ZIP level, if we want to do so for visualizations etc.

census_variables <- c("B25032_013",   # Total renter-occupied housing units 
                      "B25008_003",   # Total population in renter-occupied housing units
                      "B25119_003",   # Median household income (renter-occupied) 
                      "B25037_003",   # Median year structure built (renter-occupied) 
                      "B25039_003",   # Median year householder moved into unit (renter-occupied)
                      "B25064_001",   # Median gross rent (dollars)
                      "B25071_001",   # Median gross rent as percentage of household income in past 12 months (i.e. rent burden)
                      "B25013_011",   # Total renter-occupied housing units where householder has bachelors' degree or higher
                      "B25003H_003",  # Total rent-occ units where householder is white alone, not Hispanic or Latino
                      "B25003B_003",  # Total rent-occ units where householder is Black alone
                      "B25003D_003",  # Total rent-occ units where householder is Asian alone
                      "B25003I_003"   # Total rent-occ units where householder is Hispanic or Latino
                      )  

acs5_zctas_12_20 <- map_dfr(
  years,
  ~ get_acs(
    geography = "zcta",
    variables = census_variables, # all in ACS Occupied Housing universe as of 2023-12-08, though I'd like to add some that aren't
    year = .x,
    survey = "acs5",
    output = "wide",
    geometry = FALSE   # can set to "T" for sf mapping down the line
  ),
  .id = "year"  
) %>%
  select(-NAME, -ends_with("M")) %>% 
  mutate(year = as.numeric(year),
         GEOID = case_when(
           year == "2012" ~ str_remove(GEOID, "^36+"),  # for some reason tidycensus keeps returning the 2012 zctas with a leading "36" from the longer FIPS code, but not successive years
           TRUE ~ GEOID)
  )%>% 
  arrange(GEOID, year)

# Trim to just New York City ZCTAs (above gives nationwide) --------------------

zctas <- read_csv("data_build/2010_nyc_zctas.csv") %>% 
  mutate(GEOID = as.character(ZCTA))


zctas <- read_csv("data_raw/nyc_zip_codes.csv") %>%
  clean_names() %>% 
  mutate(GEOID = as.character(jurisdiction_name))

nyc_acs5_zctas_12_20 <- acs5_zctas_12_20 %>% 
  semi_join(zctas, by = "GEOID") %>% 
  rename(renter_occ_units = B25032_013E,
         tot_pop_rou = B25008_003E,
         med_hh_inc_rou = B25119_003E,
         med_yr_blt_rou = B25037_003E,
         med_yr_moved_in_rou = B25039_003E,
         med_gross_rent = B25064_001E,
         med_rent_burden = B25071_001E,
         tot_college_degree_rou = B25013_011E,
         tot_wh_rou = B25003H_003E,
         tot_bl_rou = B25003B_003E,
         tot_asn_rou = B25003D_003E,
         tot_ltx_rou = B25003I_003E
         ) %>% 
  mutate(med_gross_rent_2020_adj = adjust_for_inflation(med_gross_rent, year, "US", to_date = 2020),
         med_gross_rent_2020_adj = round_to_nearest(med_gross_rent_2020_adj, 1), .after = med_gross_rent) %>% 
  mutate(med_hh_inc_rou_2020_adj = adjust_for_inflation(med_hh_inc_rou, year, "US", to_date = 2020),
         med_hh_inc_rou_2020_adj = round_to_nearest(med_hh_inc_rou_2020_adj, 1), .after = med_hh_inc_rou) %>% 
  mutate(pct_college_deg_rou = round(tot_college_degree_rou/renter_occ_units, 2), .after = tot_college_degree_rou) %>% 
  mutate(pct_wh_rou = round(tot_wh_rou/renter_occ_units, 2), .after = tot_wh_rou) %>% 
  mutate(pct_bl_rou = round(tot_bl_rou/renter_occ_units, 2), .after = tot_bl_rou) %>% 
  mutate(pct_asn_rou = round(tot_asn_rou/renter_occ_units, 2), .after = tot_asn_rou) %>%
  mutate(pct_ltx_rou = round(tot_ltx_rou/renter_occ_units, 2), .after = tot_ltx_rou) 
  
  
  

# Save -------------------------------------------------------------------------

write_csv(nyc_acs5_zctas_12_20, "data_build/acs5_zctas_12_20_renter_occ_units.csv")


# By Tract, 2012 - 2020 --------------------------------------------------------

# counties <- c("Bronx", "Kings", "New York", "Queens", "Richmond") # so we don't get the whole of NY state in one file

# acs5_tracts_12_23 <- map_dfr(
#  years,
#  ~ get_acs(
#    geography = "tract",
#    state = "NY",
#    county = counties,
#    variables = "B25032_013",
#    year = .x,
#    survey = "acs5",
#    geometry = FALSE  # can set to "T" for sf mapping down the line
#  ),
#  .id = "year"  
#) %>%
#  select(-moe) %>%  
#  arrange(GEOID, year) 

# Note we'll need to crosswalk census tracts with ZIP and ZCTA so that they merge 
# seamlessly onto the tract numeration in Housing Maintenance Violation data. 
# See https://www.census.gov/geographies/reference-files/time-series/geo/relationship-files.html

# write_csv(acs5_tracts_12_23, "data_build/acs5_tracts_12_23_renter_occ_units.csv")

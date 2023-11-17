# -----------------------------------------------------------------------------
# Capstone Team 4: Adding ACS Occupied Rental Unit Count Estimates, by Census Tract
# 2012 - 2023 Utility-Related Violations
# 2023-11-15
# SL
# -----------------------------------------------------------------------------


# Dependent Packages ----------------------------------------------------------

library(tidyverse)  # data manipulation
library(tidycensus) # allows direct download of ACS microdata
options(tigris_use_cache = TRUE) # runs faster


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

years <- lst(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021) # for purrr looping later
counties <- c("Bronx", "Kings", "New York", "Queens", "Richmond") # so we don't get the whole of NY state in one file

# By Tract, 2012 - 2023 --------------------------------------------------------

acs5_tracts_12_23 <- map_dfr(
  years,
  ~ get_acs(
    geography = "tract",
    state = "NY",
    county = counties,
    variables = "B25032_013",
    year = .x,
    survey = "acs5",
    geometry = FALSE  # can set to "T" for sf mapping down the line
  ),
  .id = "year"  
) %>%
  select(-moe) %>%  
  arrange(GEOID, year) 

# Note we'll need to crosswalk census tracts with ZIP and ZCTA so that they merge 
# seamlessly onto the tract numeration in Housing Maintenance Violation data. 
# See https://www.census.gov/geographies/reference-files/time-series/geo/relationship-files.html


# By ZCTA, 2012 - 2023 --------------------------------------------------------

# "ZCTA" maps very closely to ZIP code, but isn't identical because ZIP codes are 
# not technically proper spatial boundaries. We can crosswalk the former to the 
# latter as we keep working; that allows us to estimate violation rates at both 
# the tract and the ZIP level, if we want to do so for visualizations etc.

# Using same estimate of occupied renter units as above, just at ZIP/ZCTA level:

acs5_zctas_12_23 <- map_dfr(
  years,
  ~ get_acs(
    geography = "zcta",
    variables = "B25032_013",
    year = .x,
    survey = "acs5",
    geometry = FALSE   # can set to "T" for sf mapping down the line
  ),
  .id = "year"  
) %>%
  select(-moe, -NAME) %>%
  rename(renter_occ_units = estimate) %>% 
  mutate(year = as.numeric(year),
         zip = as.numeric(GEOID)) %>% 
  arrange(GEOID, year)

rm (years, counties)

# Trim to just New York City ZCTAs (above gives nationwide) --------------------

# Again: sketch! We'll do this properly via more official source merge, but here's
# a quick version to get us looking at the data:

zctas <- read_csv("data_build/2010_nyc_zctas.csv") %>% 
  mutate(GEOID = as.character(ZCTA))

acs5_zctas_12_23 <- acs5_zctas_12_23 %>% 
  semi_join(zctas, by = "GEOID")

# Save -------------------------------------------------------------------------

write_csv(acs5_tracts_12_23, "data_build/acs5_tracts_12_23_renter_occ_units.csv")
write_csv(acs5_zctas_12_23, "data_build/acs5_zctas_12_23_renter_occ_units.csv")

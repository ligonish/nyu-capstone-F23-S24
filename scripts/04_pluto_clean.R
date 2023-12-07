# ------------------------------------------------------------------------------
# PLUTO Cleaning
# Date: 2023-12-05
# Author: TB
# ------------------------------------------------------------------------------

# Packages ---------------------------------------------------------------------

library(tidyverse)
library(RSocrata)

# API Access -------------------------------------------------------------------

# see 00_get_violations.R for an in-depth walkthrough on API access

token <- "XXXXXXXXX"

complaint_url <- "https://data.cityofnewyork.us/resource/ygpa-z7cr.csv?$select=BBL&$group=BBL&$order=BBL"
complaint_df <- read.socrata(url = complaint_url, app_token = token)
  
# Complaint Clean --------------------------------------------------------------
# using 20 years of complaint data to approximate rental status 

complaint_flag <- complaint_df %>%
  filter(BBL != 0) %>%
  mutate(possible_rental = 1)

# PLUTO Clean  -----------------------------------------------------------------
# using data reshaped from the database of the NYU Furman Center





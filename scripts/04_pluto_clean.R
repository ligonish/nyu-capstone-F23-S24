# ------------------------------------------------------------------------------
# PLUTO Cleaning
# Date: 2023-12-05
# Author: TB
# ------------------------------------------------------------------------------

# Packages ---------------------------------------------------------------------

library(tidyverse)
library(RSocrata)
library(here)

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

raw_furman_pluto <- read.csv("/Users/tonybodulovic/Desktop/аспирантура/capstone/pluto_furman_pull.csv")
  
pluto_join <- raw_furman_pluto %>%
  select(-X) %>%
  filter(unitsres > 0) %>%
  left_join(complaint_flag, by = c("bbl" = "BBL"))

# flags provide information on 1-2 family properties to include as rentals

flag_counts <- pluto_join %>%
  group_by(bldgclass) %>%
  summarize(count = sum(possible_rental, na.rm = T))

# units by class

unit_by_class <- pluto_join %>%
  group_by(bldgclass) %>%
  summarize(sum = sum(unitsres, na.rm = T))

# dropping condos/coops/other classes

classes_to_drop <- c("A0", "A3", "A6", "A7", "A8", "C6", "C8", "CM", "D0", "DC", "D4", "D5", 
                     "D9", "O1", "O2", "O3", "O4", "O5", "O6", "O7", "O9",
                     "K1", "K2", "K3", "K5", "K6", "K7", "K8", "K9")

pluto_clean <- pluto_join %>%
  mutate(
    borough = case_when(
      borough == "MN" ~ 1,
      borough == "BX" ~ 2,
      borough == "BK" ~ 3,
      borough == "QN" ~ 4,
      borough == "SI" ~ 5),
    tract_2010 = paste(borough, ct2010, sep = "") # unique tract numbers for each boro
    ) %>%
  filter(yearbuilt < 2013) %>% # we want to keep the same # of units across the time series, so dropping
  # anything built after 2012
  select(-borough, -ct2010) %>%
  filter(!str_detect(bldgclass, "^Z") & !str_detect(bldgclass, "^Y") & !str_detect(bldgclass, "^W") &
         !str_detect(bldgclass, "^V") & !str_detect(bldgclass, "^U") & !str_detect(bldgclass, "^T") &
         !str_detect(bldgclass, "^R") & !str_detect(bldgclass, "^Q") & !str_detect(bldgclass, "^N") &
         !str_detect(bldgclass, "^M") & !str_detect(bldgclass, "^L") & !str_detect(bldgclass, "^J") &
         !str_detect(bldgclass, "^I") & !str_detect(bldgclass, "^H") & !str_detect(bldgclass, "^G") &
         !str_detect(bldgclass, "^F") & !str_detect(bldgclass, "^E")) %>%
  filter(!bldgclass %in% classes_to_drop) %>% # dropping building classes like hotels, theatres, offices, etc 
  filter(!(str_detect(bldgclass, '^A') & is.na(possible_rental))) %>%
  filter(!(str_detect(bldgclass, '^B') & is.na(possible_rental)))

sum(pluto_clean$unitsres)

write.csv(pluto_clean, here("data_build", "pluto_clean.csv"))
    
# by filtering on the prior conditions, we arrive at 1.93 million rental units, which is remarkably close
# to the 2.1 million rental units estimated by HVS in their 2011 survey

# our estimate is conservative as it requires a logged complaint from a 1-2 family unit sometime in
# the last 20 years to merit inclusion in the dataframe

pluto_zip_tract <- pluto_clean %>%
  group_by(tract_2010, zipcode) %>%
  summarize(unit_count_2012 = sum(unitsres)) %>%
  arrange(tract_2010)

write.csv(pluto_zip_tract, here("data_build", "pluto_zip_tract.csv"))

pluto_zip_only <- pluto_clean %>%
  group_by(zipcode) %>%
  summarize(unit_count_2012 = sum(unitsres)) %>%
  arrange(zipcode)

write.csv(pluto_zip_only, here("data_build", "pluto_zip_tract.csv"))


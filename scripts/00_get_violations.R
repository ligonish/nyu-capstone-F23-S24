# -----------------------------------------------------------------------------
# Initial Capstone Data Pull: Testing API Method
# Date: 2023-11-14
# Author: SL
# Source: https://nycopendata.socrata.com/Housing-Development/Housing-Maintenance-Code-Violations/wvxf-dwi5
# See documentation folder for data dictionaries 
# -----------------------------------------------------------------------------


# Dependent Packages ----------------------------------------------------------

library(tidyverse)  # cleaner data manipulation
library(RSocrata)   # pulls from NYC OpenData API


# Set Up API Access to NYC OpenData -------------------------------------------

# To use this method, you'll need your own free app token to access NYC OpenData APIs.
# You can get one via https://data.cityofnewyork.us/profile/edit/developer_settings
# Once you have it, store locally on your machine in place of the "XXXXXXXXX" value below.

token <- "XXXXXXXXXXXXXXXXXXXX"

# Now visit https://nycopendata.socrata.com/Housing-Development/Housing-Maintenance-Code-Violations/wvxf-dwi5 
# and click the "API" tab. Choose "CSV" for API Endpoint and copy the resulting URL.
# which is https://data.cityofnewyork.us/resource/wvxf-dwi5.csv

# The above URL contains 8.5 million rows, going all the way back to the early 1990s. We only need data from 2016 onward, so let's simplify:
# See https://dev.socrata.com/docs/queries/ for list of Socrata queries for filtering; 
# the syntax is SQL-like. Tutorial at https://hwangnyc.medium.com/using-r-to-access-311-service-request-from-nyc-open-data-using-socrata-open-data-api-and-the-83de00327a8c
# Further detail on Housing Maintenance Code Violation storage & structure at 
# https://dev.socrata.com/foundry/data.cityofnewyork.us/wvxf-dwi5 


# Pull Subset: Heat/HW/Gas Violations, 2012 - Present -------------------------

url <- "https://data.cityofnewyork.us/resource/wvxf-dwi5.json?$where= inspectiondate >= '2012-01-01T00:00:00.000' and (ordernumber = '666' or ordernumber = '664' or ordernumber = '966' or ordernumber = '964' or ordernumber = '670' or ordernumber = '970' or ordernumber = '742' or ordernumber = '577' or ordernumber = '877' or ordernumber = '510' or ordernumber = '810')"
util_violations <- read.socrata(url = url, app_token = token) 
     # NB: we'll keep refining violation code selection here, and repeat with all Class C in batches/compressed


# Save in Compressed Form  ----------------------------------------------------

write_csv(util_violations, "data_raw/utility_violations_2012_onward.csv.gz")


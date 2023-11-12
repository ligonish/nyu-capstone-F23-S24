# -----------------------------------------------------------------------------
# Initial Capstone Data Pull (Testing)
# Date: 2023-11-11
# Author: SL
# Source: https://nycopendata.socrata.com/Housing-Development/Housing-Maintenance-Code-Violations/wvxf-dwi5
# See documentation folder for data dictionaries 
# -----------------------------------------------------------------------------


# Dependent Packages 

# If necessary, first run install.packages("RSocrata")
library(RSocrata)   # pulls from NYC OpenData API


# Set Up API Access to NYC OpenData 

# To use this method, you'll need your own free app token to access NYC OpenData APIs.
# You can get one via https://data.cityofnewyork.us/profile/edit/developer_settings
# Once you have it, store locally on your machine in place of the "XXXXXXXXX" value below.

token <- "XXXXXXXXXXXXXXX"

# Now visit https://nycopendata.socrata.com/Housing-Development/Housing-Maintenance-Code-Violations/wvxf-dwi5 
# and click the "API" tab. Choose "CSV" for API Endpoint and copy the resulting URL.
# which is https://data.cityofnewyork.us/resource/wvxf-dwi5.csv

# The above URL contains 8.5 million rows, going all the way back to the early 1990s. We only need data from 2016 onward, so let's simplify:
# See https://dev.socrata.com/docs/queries/ for list of Socrata queries for filtering; tutorial at https://hwangnyc.medium.com/using-r-to-access-311-service-request-from-nyc-open-data-using-socrata-open-data-api-and-the-83de00327a8c
# See also https://dev.socrata.com/foundry/data.cityofnewyork.us/wvxf-dwi5 


# Rent-Impairing Class C Violations 

# Calling all Class C or B violations since January 1, 2012 gives over 3 million rows of data: a massive file! 
# For initial exploration on home machines, I narrowed it down to the worst of the worst: 
# Class C violations deemed "rent-impairing" (hazardous to life/safety).
# See New York State Multiple Dwelling Law Section 302: "A rent-impairing violation is a condition within a multiple dwelling which constitutes, or if not promptly corrected will constitute, a fire hazard or a serious threat to the life, health or safety of occupants thereof."

url <- "https://data.cityofnewyork.us/resource/wvxf-dwi5.json?$where= inspectiondate >= '2012-01-01T00:00:00.000' and class = 'C' and rentimpairing = 'Y'"
ri_c_violations <- read.socrata(url = url, app_token = token)


# Save 

write_csv(ri_c_violations, "data_raw/rent_impairing_class_c_violations_2012_to_2023.csv")


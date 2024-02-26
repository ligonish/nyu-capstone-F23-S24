# -----------------------------------------------------------------------------
# Capstone Team 4
# Staggered Difference in Differences Analysis (Callaway & Sant'Anna)
# 2024-02-19
# SL
# -----------------------------------------------------------------------------

# Resources
# See https://bcallaway11.github.io/did/articles/did-basics.html (2023-08-29)

# Dependent Packages ----------------------------------------------------------

library(did)  # Callaway & Sant'Anna's package implementing their staggered DiD
library(tidyverse) 
library(janitor)
library(lubridate)
library(zoo)   # stores evenly-counted year-month periods as numeric variables

# Data Import -----------------------------------------------------------------

violations <- read_csv("data_build/2012_2020_zip_set_for_analysis.csv.gz")

# Via -did- package documentation: "The variable in 'gname' should be expressed as the time a unit is first treated (0 if never-treated)."
# this is messy; will streamline back in cleaning script_02 but just sketching for now

violations <- violations %>% 
  mutate(inspection_yr_mo = as.yearmon(inspection_yr_mo),
         treat_yr_mo = as.yearmon(rtc_treat_date)) %>%   # via zoo package, gives evenly-spaced yearmonth increments where january is year.00, feb is year.1/12, march is year.2/12, etc
  mutate(inspection_yr_mo_num = as.numeric(inspection_yr_mo), .after = inspection_yr_mo) %>% 
  mutate(treat_yr_mo_num = as.numeric(treat_yr_mo), .after = treat_yr_mo) %>% 
  filter(inspection_yr_mo >= 2013) # HUD doesn't have full set of violations for 2012. 15,480 obs of 27 variables, 2013-2023

# Check for missing data in newer baseline eviction/poverty/rent stabilization variables ---

empties <- violations %>%   # violation rate missing values
  filter(is.na(n_violations_per_1k_units)) 
empties %>% 
  group_by(zip) %>% 
  summarize(records = n()) # ZIP 11249 missing all ACS variables, Jan. 2013 - Feb. 2020 (86 obs)

empties <- violations %>%  
  filter(is.na(evict_rate_17)) 
empties %>% 
  group_by(zip) %>% 
  summarize(records = n()) # ZIPs 11001 and 11249 missing eviction rates

empties <- violations %>%  
  filter(is.na(rs_rate_17)) 
empties %>% 
  group_by(zip) %>% 
  summarize(records = n()) # ZIPs 10309, 10312, 11239, 11249, & 11436 missing rent stabilization rates


# Check building-level records to see where these were intro'd to our data

og_violations <- read_csv("data_build/cleaner_utility_violations.csv.gz")

# After hand-checking street addresses, three of these ZIPs resulted from obvious HUD data entry errors in original violation records.
#     "10129" mis-entered for 10128 
#     "10435" mis-entered for 11435
#     "11452" mis-entered for 10452.
# 11249, 10006, & 10464 are all correctly-entered ZIP codes, but for some reason their census data is incomplete for 2015 or 2016 only.
#     Since -did- uses pre-treat data as baseline, this may not matter.
#     2024-02-19 SL update: went back to script 01_get_census and imputed missing estimate using previous census year's estimate for the missing 2015 or 2016 value. 

# GO BACK AND RECODE THESE IN CLEANING SCRIPT.
# For now, I'm going to drop them just to get the analysis code working; consider this dummy data, ish

# Drop faulty ZIP records (temporary measure)

violations <- violations %>% 
  filter(zip != 11249)

rm(og_violations, empties)

# Group-Time ATE, 2013-2020: No Covariates -----------------------------------------------

est_test <- att_gt(yname = "n_violations_per_1k_units",   # outcome 
                   gname = "treat_yr_mo_num",
                   idname = "zip",
                   tname = "inspection_yr_mo_num",
                   xformla = ~1, # doing this or leaving blank sets as a constant w/o covars; see "Getting Started"
                   data = violations,
                   allow_unbalanced_panel = TRUE,
                   #base_period = "varying",
                   control_group = "notyettreated",
                   est_method = "dr") 
# Warning: "In att_gt(yname = "n_violations_per_1k_units", gname = "treat_yr_mo",  :
#     "Not returning pre-test Wald statistic due to singular covariance matrix"

summary(est_test) # not looking statistically significant
ggdid(est_test,
      title = "Group-Time Average Treatment Effects of Right-to-Counsel on Landlord Maintenance Violations, 2013-2020",
      grtitle = "Received UA",
      xgap = 12)

# Event Study w/ Plot 
# This aggregates the group-time ATEs
event_study <- aggte(est_test, type = "dynamic")
summary(event_study) # results not statistically significant
ggdid(event_study)

# Overall Effect of Participating in Treatment

group_effects <- aggte(est_test, type = "group")
summary(group_effects) # results not statistically significant
ggdid(group_effects)

# 2013-2023, but now including covariates --------------------------------------

est_w_covars <- att_gt(yname = "n_violations_per_1k_units",   # outcome 
                       gname = "treat_yr_mo_num",
                       idname = "zip",
                       tname = "inspection_yr_mo_num",
                       xformla = ~ evict_rate_17 + rs_rate_17 + pct_pov_17, # controlling for baseline 2017 eviction rates, rent-stabilization rates, & percentage below Fed poverty line
                       data = violations,
                       control_group = "notyettreated",
                       # base_period = "varying",
                       allow_unbalanced_panel = TRUE,
                       est_method = "dr") # dr = doubly robust; ipw = inverse probability weighting; reg = regression 
# Warning: "dropped 430 rows from original data due to missing data" - check all covars again
summary(est_w_covars)
ggdid(est_w_covars,
      title = "Group-Time Average Treatment Effects of Right-to-Counsel on Landlord Maintenance Violations, 2013-2020",
      grtitle = "Received UA",
      xgap = 50)

# Overall effect of participating in treatment 
group_effects <- aggte(est_w_covars, type = "group")
summary(group_effects) # results not statistically significant 
ggdid(group_effects)

group_effects <- aggte(est_w_covars, type = "dynamic")
summary(group_effects) # results not statistically significant
ggdid(group_effects)

group_effects <- aggte(est_w_covars, type = "simple")
summary(group_effects) # results not statistically significant

group_effects <- aggte(est_w_covars, type = "calendar")
summary(group_effects) # results not statistically significant

# Group-Time ATE, Jan. 2013 - May 2019: No Covariates -----------------------------------------------

# Filter out HSTPA-onward observations (seeing what happens if we skip COVID & most of the impact of 2019's legislative changes)

violations_13_19 <- violations %>% 
  filter(inspection_yr_mo <= 2019.417) 

est_test <- att_gt(yname = "n_violations_per_1k_units",   # outcome 
                   gname = "treat_yr_mo_num",
                   idname = "zip",
                   tname = "inspection_yr_mo_num",
                   xformla = ~1, # doing this or leaving blank sets as a constant w/o covars; see "Getting Started"
                   data = violations_13_19,
                   allow_unbalanced_panel = TRUE,
                   #base_period = "varying",
                   control_group = "notyettreated",
                   est_method = "dr") 
# Warning: In att_gt(yname = "n_violations_per_1k_units", gname = "treat_yr_mo",  :
#     Not returning pre-test Wald statistic due to singular covariance matrix
#     See https://www.sciencedirect.com/science/article/pii/S0047259X17302701 for potential alternative to Wald 

summary(est_test)
ggdid(est_test,
      title = "Group-Time Average Treatment Effects of Right-to-Counsel on Landlord Maintenance Violations, 2013-2020",
      grtitle = "Received UA",
      xgap = 11)

# Event Study w/ Plot 
# This aggregates the group-time ATEs
event_study <- aggte(est_test, type = "dynamic")
summary(event_study) # results not statistically significant
ggdid(event_study)

# Overall Effect of Participating in Treatment

group_effects <- aggte(est_test, type = "group")
summary(group_effects) # results not statistically significant
ggdid(group_effects)

# 2013-2019, WITH covariates -----------------------------------------

est_w_covars <- att_gt(yname = "n_violations_per_1k_units",   # outcome 
                       gname = "treat_yr_mo_num",
                       idname = "zip",
                       tname = "inspection_yr_mo_num",
                       xformla = ~ evict_rate_17 + rs_rate_17 + pct_pov_17, # controlling for baseline 2017 eviction rates, rent-stabilization rates, & percentage below Fed poverty line
                       data = violations_13_19,
                       control_group = "notyettreated",
                       #base_period = "varying",
                       #allow_unbalanced_panel = TRUE,
                       est_method = "dr") # dr = doubly robust; ipw = inverse probability weighting; reg = regression 
# 390 rows dropped due to missing data
summary(est_w_covars)
ggdid(est_w_covars,
      title = "Group-Time Average Treatment Effects of Right-to-Counsel on Landlord Maintenance Violations, 2013-2020",
      grtitle = "Received UA",
      xgap = 20)

group_effects <- aggte(est_w_covars, type = "group")
summary(group_effects) # results not statistically significant 
ggdid(group_effects)

group_effects <- aggte(est_w_covars, type = "dynamic")
summary(group_effects) # results not statistically significant
ggdid(group_effects)

group_effects <- aggte(est_w_covars, type = "simple")
summary(group_effects) # results not statistically significant

group_effects <- aggte(est_w_covars, type = "calendar")
summary(group_effects) # results not statistically significant


# Outstanding questions:
# Do we need to renumber the yearmonths so expressed as periods/increments? 
# Consistently getting warning "Not returning pre-test Wald statistic due to singular covariance matrix"

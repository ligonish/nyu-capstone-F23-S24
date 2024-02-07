# -----------------------------------------------------------------------------
# Capstone Team 4
# Staggered Difference in Differences Analysis (Callaway & Sant'Anna)
# 2024-02-05
# SL
# -----------------------------------------------------------------------------

# Resources
# See https://bcallaway11.github.io/did/articles/did-basics.html (2023-08-29)

# Dependent Packages ----------------------------------------------------------

library(did)  # Callaway & Sant'Anna's package implementing their staggered DiD
library(tidyverse) 
library(janitor)
library(lubridate)

# Data Import -----------------------------------------------------------------

violations <- read_csv("data_build/2012_2020_zip_set_for_analysis.csv.gz")

# Via -did- package documentation: "The variable in 'gname' should be expressed as the time a unit is first treated (0 if never-treated)."
# this is messy; will streamline back in cleaning script_02 but just sketching for now

violations <- violations %>% 
  mutate(
    inspection_yr_mo = as.character(str_sub(inspection_yr_mo, end = -4)),
    inspection_yr_mo = str_replace(inspection_yr_mo, "-", "."),
    inspection_yr_mo = as.numeric(inspection_yr_mo),
      treat_yr_mo = as.character(str_sub(rtc_treat_date, end = -4)),
      treat_yr_mo = str_replace(treat_yr_mo, "-", "."),
      treat_yr_mo = as.numeric(treat_yr_mo),
      treat_yr_mo = case_when(
        treat_yr_mo == 2021.05 ~ 0, # sets final cohort as untreated, since we're trimming timeline 
        TRUE ~ treat_yr_mo),
  ) %>% 
  filter(inspection_yr_mo > 2012.12) # HUD doesn't have full set of violations for 2012. 15,480 obs of 27 variables, 2013-2023

empties <- violations %>% 
  filter(is.na(n_violations_per_1k_units)) # 344 missing

empties %>% 
  group_by(zip) %>% 
  summarize(records = n())
  # 10129, 10435, 11249, & 11452 missing 86 obs each (= 344)


# Check building-level records to see where these were intro'd to our data

og_violations <- read_csv("data_build/cleaner_utility_violations.csv.gz")

# After hand-checking street addresses, three of these ZIPs resulted from obvious HUD data entry errors in original violation records.
#     "10129" mis-entered for 10128 
#     "10435" mis-entered for 11435
#     "11452" mis-entered for 10452.
# 11249, 10006, & 10464 are all correctly-entered ZIP codes, but for some reason their census data is incomplete in certain years.
#     Since -did- uses pre-treat data as baseline, this may not matter 

# GO BACK AND RECODE THESE IN CLEANING SCRIPT.
# For now, I'm going to drop them just to get the analysis code working; consider this dummy data, ish

# Drop faulty ZIP records (temporary measure)

violations <- violations %>% 
  filter(!zip %in% c(10129, 10435, 11249, 11452)) # 15,136 obs of 27 vars
  

# Group-Time ATE, 2013-2023: No Covariates -----------------------------------------------

est_test <- att_gt(yname = "n_violations_per_1k_units",   # outcome 
              gname = "treat_yr_mo",
              idname = "zip",
              tname = "inspection_yr_mo",
              xformla = ~1, # doing this or leaving blank sets as a constant w/o covars; see "Getting Started"
              data = violations,
              #allow_unbalanced_panel = TRUE,
              control_group = "notyettreated",
              est_method = "dr") 
# Warning: "In att_gt(yname = "n_violations_per_1k_units", gname = "treat_yr_mo",  :
#     "Not returning pre-test Wald statistic due to singular covariance matrix"

summary(est_test) # not looking statistically significant
ggdid(est_test) 

# Event Study w/ Plot 
# This aggregates the group-time ATEs
event_study <- aggte(est_test, type = "dynamic")

summary(event_study) # results not statistically significant

ggdid(event_study)

# Overall Effect of Participating in Treatment

group_effects <- aggte(est_test, type = "group")

summary(group_effects) # results not statistically significant

# As above, but now including covariates

est_w_covars <- att_gt(yname = "n_violations_per_1k_units",   # outcome 
                   gname = "treat_yr_mo",
                   idname = "zip",
                   tname = "inspection_yr_mo",
                   xformla = ~ med_hh_inc_rou_2020_adj + med_yr_blt_rou + pct_wh_rou + pct_college_deg_rou, # controlling for AMI, building age, and percentage of renters who are white
                   data = violations,
                   control_group = "notyettreated",
                   #base_period = "varying",
                   #allow_unbalanced_panel = TRUE,
                   est_method = "dr") # dr = doubly robust; ipw = inverse probability weighting; reg = regression 
     # this throws warning: "Be aware that there are some small groups in your dataset. Check groups: 2017.1,2018.11,2019.12.
summary(est_w_covars)
ggdid(est_w_covars)

group_effects <- aggte(est_w_covars, type = "group")
summary(group_effects) # results not statistically significant 

group_effects <- aggte(est_w_covars, type = "dynamic")
summary(group_effects) # results not statistically significant

group_effects <- aggte(est_w_covars, type = "simple")
summary(group_effects) # results not statistically significant

group_effects <- aggte(est_w_covars, type = "calendar")
summary(group_effects) # results not statistically significant

# Group-Time ATE, 2013-2019: No Covariates -----------------------------------------------

# Filter out 2020-onward observations (seeing what happens if we skip COVID & most of the impact of 2019's legislative changes)

violations_13_19 <- violations %>% 
  filter(inspection_yr_mo <= 2019.12) # 14,784 obs of 27 vars

est_test <- att_gt(yname = "n_violations_per_1k_units",   # outcome 
                   gname = "treat_yr_mo",
                   idname = "zip",
                   tname = "inspection_yr_mo",
                   xformla = ~1, # doing this or leaving blank sets as a constant w/o covars; see "Getting Started"
                   data = violations_13_19,
                   #allow_unbalanced_panel = TRUE,
                   #base_period = "varying",
                   control_group = "notyettreated",
                   est_method = "dr") # error: "dropped 344 rows from original data due to missing data" (?)
# Warning: In att_gt(yname = "n_violations_per_1k_units", gname = "treat_yr_mo",  :
#     Not returning pre-test Wald statistic due to singular covariance matrix

summary(est_test)

ggdid(est_test)

# Event Study w/ Plot 
# This aggregates the group-time ATEs
event_study <- aggte(est_test, type = "dynamic")

summary(event_study) # results not statistically significant

ggdid(event_study)

# Overall Effect of Participating in Treatment

group_effects <- aggte(est_test, type = "group")

summary(group_effects) # results not statistically significant

# As above, but now including covariates

est_w_covars <- att_gt(yname = "n_violations_per_1k_units",   # outcome 
                       gname = "treat_yr_mo",
                       idname = "zip",
                       tname = "inspection_yr_mo",
                       xformla = ~ med_hh_inc_rou_2020_adj + med_yr_blt_rou + pct_wh_rou, # controlling for AMI, building age, and percentage of renters who are white
                       data = violations_13_19,
                       control_group = "notyettreated",
                       #base_period = "varying",
                       #allow_unbalanced_panel = TRUE,
                       est_method = "dr") # dr = doubly robust; ipw = inverse probability weighting; reg = regression 
# this throws warning: "Be aware that there are some small groups in your dataset. Check groups: 2017.1,2018.11,2019.12.
summary(est_w_covars)
ggdid(est_w_covars)

group_effects <- aggte(est_w_covars, type = "group")
summary(group_effects) # results not statistically significant 

group_effects <- aggte(est_w_covars, type = "dynamic")
summary(group_effects) # results not statistically significant

group_effects <- aggte(est_w_covars, type = "simple")
summary(group_effects) # results not statistically significant

group_effects <- aggte(est_w_covars, type = "calendar")
summary(group_effects) # results not statistically significant


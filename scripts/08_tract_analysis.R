# -----------------------------------------------------------------------------
# Capstone Team 4
# Tract-Level Staggered Difference in Differences Analysis (Callaway & Sant'Anna)
# 2024-03-24
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

violations <- read_csv("data_build/tract_set_for_analysis.csv.gz")

# Via -did- package documentation: "The variable in 'gname' should be expressed as the time a unit is first treated (0 if never-treated)."
# this is messy; will streamline back in cleaning script_02 but just sketching for now

violations <- violations %>% 
  mutate(inspection_yr_mo = as.yearmon(inspection_yr_mo),
         treat_yr_mo = as.yearmon(rtc_treat_date)) %>%   # via zoo package, gives evenly-spaced yearmonth increments where january is year.00, feb is year.1/12, march is year.2/12, etc
  mutate(inspection_yr_mo_num = as.numeric(inspection_yr_mo), .after = inspection_yr_mo) %>% 
  mutate(treat_yr_mo_num = as.numeric(treat_yr_mo), .after = treat_yr_mo) %>% 
  mutate(treat_yr_mo_zero = case_when(
    treat_yr_mo == "May 2021" ~ as.numeric(0),
    treat_yr_mo == "Dec 2019" ~ as.numeric(0),
    TRUE ~ treat_yr_mo_num)) %>%   # 160,391 obs. of 44 variables
  mutate_all(~ifelse(is.nan(.), NA, .)) %>% 
  mutate_all(~ifelse(is.infinite(.), NA, .))  # converts NaN & infinite values to NA, since -did- package can't parse the former as the latter


# Check for missing data in newer baseline eviction/poverty/rent stabilization variables ---

empties <- violations %>%   # violation rate missing values
  filter(is.na(n_violations_per_1k_units)) # 17,541 of 160,391 ()
empties %>% 
  group_by(geoid) %>% 
  summarize(records = n()) # 228 tracts are missing n_violations_per_1k units

empties <- violations %>%  
  filter(is.na(evict_rate_17)) # 34,573 of 160,391 missing eviction rate
empties %>% 
  group_by(geoid) %>% 
  summarize(records = n()) # 449 tracts missing eviction rate

empties <- violations %>%  
  filter(is.na(rs_rate_17)) # 40,348 obs missing rent stabilization rate
empties %>% 
  group_by(geoid) %>% 
  summarize(records = n()) # 524 obs of 160,391 missing rent stabilization rate

# ...

# TEMPORARY: DROP NA VALUES IN OUTCOME VARIABLE (n_violations_per_1k_units)
# violations <- violations %>%   #160,391 obs
#  drop_na() #105,325
#  drop_na(n_violations_per_1k_units)   #142,850 obs



# Group-Time ATE, Jan. 2013 - May 2019: No Covariates -----------------------------------------------

# Filter out HSTPA-onward observations (seeing what happens if we skip COVID & most of the impact of 2019's legislative changes)

est_test <- att_gt(yname = "n_violations_per_1k_units",    
                   gname = "treat_yr_mo_zero",      # year-month first treated
                   idname = "geoid",
                   tname = "inspection_yr_mo_num",  # year-month time periods
                   xformla = ~1, 
                   data = violations,
                   allow_unbalanced_panel = TRUE,   # prevents model from forcibly balancing panel by dropping observations 
                   control_group = "notyettreated", # controls are both not-yet-treated within 2013-2019 (Cohorts 2 & 3) and never-treated until after 2019 (Cohorts 4 & 5)
                   est_method = "dr")   # doubly-robust standard errors 
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
summary(group_effects) # Wave 1 shows statistically significant update w/o covariates (March, before recouping all the dropped NAs)
ggdid(group_effects)

# 2013-2019, WITH covariates -----------------------------------------

maj_wh <- violations %>% 
  filter(pct_wh_17 > 0.5) # 44,968

maj_poc <- violations %>% 
  filter(pct_wh_17 < 0.5) # 96,943
  
est_w_covars <- att_gt(yname = "n_violations_per_1k_units",  
                       gname = "treat_yr_mo_num",
                       idname = "geoid",
                       tname = "inspection_yr_mo_num",
                       xformla = ~ evict_rate_17 + rs_rate_17 + pct_pov_17, 
                       data = violations,
                       control_group = "notyettreated",
                       allow_unbalanced_panel = TRUE,
                       est_method = "dr") 
summary(est_w_covars)
ggdid(est_w_covars,
      title = "Group-Time Average Treatment Effects of Right-to-Counsel on Landlord Maintenance Violations, 2013-2020",
      grtitle = "Received UA",
      xgap = 20)

group_effects <- aggte(est_w_covars, type = "group")
summary(group_effects) # Wave 1 has a statistically significant bump in hhw violations 
ggdid(group_effects)
   # All: Wave 1 has a +0.35 increase (ss), 2 has -0.20 (nss), 3 has -0.06(nss)
   # Maj Wh: Wave 1 -0.01(nss), Wave 2 -0.27(nss), Wave 3 dropped entirely
   # Maj POC: Wave 1 +0.31 (ss), 2 has -0.26 (nss), 3 has -0.12(nss)

group_effects <- aggte(est_w_covars, type = "dynamic")
summary(group_effects) # results not statistically significant
ggdid(group_effects,
      xgap = 20)

group_effects <- aggte(est_w_covars, type = "simple")
summary(group_effects) # results not statistically significant
  #Maj Wh: -0.07 (nss)
  #Maj POC: +0.11 (nss)
  #All: +0.13

group_effects <- aggte(est_w_covars, type = "calendar")
summary(group_effects) # results not statistically significant



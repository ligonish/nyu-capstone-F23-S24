# -----------------------------------------------------------------------------
# Capstone Team 4
# Tract-Level Staggered Difference in Differences Analysis (Callaway & Sant'Anna)
# 2024-03-24
# SL
# -----------------------------------------------------------------------------

# Resources
# See https://bcallaway11.github.io/did/articles/did-basics.html (2023-08-29)

options(scipen=999) # removes scientific notation in GEOID field etc

# Dependent Packages ----------------------------------------------------------

library(did)  # Callaway & Sant'Anna's package implementing their staggered DiD
library(tidyverse) 
library(janitor)
library(lubridate)
library(zoo)   # stores evenly-counted year-month periods as numeric variables

# Data Import -----------------------------------------------------------------

violations <- read_csv("data_build/tract_set_for_analysis.csv.gz")

# Via -did- package documentation: "The variable in 'gname' should be expressed as the time a unit is first treated (0 if never-treated)."

violations <- violations %>% 
  mutate(inspection_yr_mo = as.yearmon(inspection_yr_mo),
         treat_yr_mo = as.yearmon(rtc_treat_date)) %>%   # via zoo package, gives evenly-spaced yearmonth increments where january is year.00, feb is year.1/12, march is year.2/12, etc
  mutate(inspection_yr_mo_num = round(as.numeric(inspection_yr_mo), digits = 2), .after = inspection_yr_mo) %>% 
  mutate(treat_yr_mo_num = round(as.numeric(treat_yr_mo), digits = 2), .after = treat_yr_mo) %>% 
  mutate(treat_yr_mo_zero = case_when(   # CS-DiD documentation recommends recoding cases never treated during study period (here, Cohorts 4 + 5) as "0"
    treat_yr_mo == "May 2021" ~ as.numeric(0),
    treat_yr_mo == "Dec 2019" ~ as.numeric(0),
    TRUE ~ treat_yr_mo_num)) %>%      # 160,391 obs. of 44 variables
  mutate_all(~ifelse(is.nan(.), NA, .)) %>% 
  mutate_all(~ifelse(is.infinite(.), NA, .))  # converts NaN & infinite values to NA, since -did- package can't parse the former as the latter


# Check for missing data in newer baseline eviction/poverty/rent stabilization variables ---

empties <- violations %>%   # violation rate missing values
  filter(is.na(n_violations_per_1k_units)) # 325 of 160,391 ()
empties %>% 
  group_by(geoid) %>% 
  summarize(records = n()) # 77 tracts are missing n_violations_per_1k units

empties <- violations %>%  
  filter(is.na(evict_rate_17)) # 17,556 of 160,391 missing eviction rate
empties %>% 
  group_by(geoid) %>% 
  summarize(records = n()) # 228 tracts missing eviction rate

empties <- violations %>%  
  filter(is.na(rs_rate_17)) # 24,717 obs missing rent stabilization rate
empties %>% 
  group_by(geoid) %>% 
  summarize(records = n()) # 321 tracts missing rent stabilization rate

# Identify non-residential census tracts 

violations %>% 
  group_by(geoid) %>% 
  distinct(geoid)  # 1,987 tracts

violations %>% 
  group_by(geoid) %>%
  filter(inspection_yr_mo >= 2017, inspection_yr_mo < 2018, tot_units > 0) %>% 
  distinct(geoid)  # 4 non-residential tracts contained zero housing units in 2017; 1,983 were residential.

# Quick summary variable confirmation, by cohort

violations %>% 
  filter(!is.na(cohort)) %>% 
  group_by(cohort) %>% 
  mutate(med_yr_blt_rou = na_if(med_yr_blt_rou, 0)) %>% 
  summarize(med_yr_blt_rou = mean(med_yr_blt_rou, na.rm = T),
            pct_college_deg_rou = mean(pct_college_deg_rou, na.rm = T),
            pct_bl_rou = mean(pct_bl_rou, na.rm = T),
            pct_ltx_rou = mean(pct_ltx_rou, na.rm = T),
            pct_asn_rou = mean(pct_asn_rou, na.rm = T),
            pct_wh_rou = mean(pct_wh_rou, na.rm = T),
            pct_wh_17 = mean(pct_wh_17, na.rm = T),
            pct_bl_17 = mean(pct_bl_17, na.rm = T),
            pct_pov_17 = mean(pct_pov_17, na.rm = T),
            evict_rate_17 = mean(evict_rate_17, na.rm = T),
            rs_rate_17 = mean(rs_rate_17, na.rm = T)
            )

violations %>% 
  mutate(med_yr_blt_rou = na_if(med_yr_blt_rou, 0)) %>% 
  summarize(med_yr_blt_rou = mean(med_yr_blt_rou, na.rm = T),
            pct_college_deg_rou = mean(pct_college_deg_rou, na.rm = T),
            pct_bl_rou = mean(pct_bl_rou, na.rm = T),
            pct_ltx_rou = mean(pct_ltx_rou, na.rm = T),
            pct_asn_rou = mean(pct_asn_rou, na.rm = T),
            pct_wh_rou = mean(pct_wh_rou, na.rm = T),
            pct_wh_17 = mean(pct_wh_17, na.rm = T),
            pct_bl_17 = mean(pct_bl_17, na.rm = T),
            pct_pov_17 = mean(pct_pov_17, na.rm = T),
            evict_rate_17 = mean(evict_rate_17, na.rm = T),
            rs_rate_17 = mean(rs_rate_17, na.rm = T)
  )
# Cohort 1 has the same mean building age as the rest of NYC (1952), but differs most in its proportion of Black residents -- on average during our study time period, 
# 56% of Cohort 1 householders are Black, compared to 27% citywide. 25% in pov compared to 18.7% citywide pctpov17; 22.4% have college degrees, which is consistent with other groups;
# 1% eviction rate and 4% rs rate, compared to the rest of the city's 4-5%.



# Tract Group-Time ATE, Jan. 2013 - May 2019: No Covariates -----------------------------------------------

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

# Tract Event Study w/ Plot, No Covariates 
# This aggregates the group-time ATEs
event_study <- aggte(est_test, type = "dynamic")
summary(event_study) # results not statistically significant
ggdid(event_study)

# Overall Effect of Participating in Treatment

group_effects <- aggte(est_test, type = "group")
summary(group_effects) # Wave 1 shows statistically significant update w/o covariates (March, before recouping all the dropped NAs)
ggdid(group_effects)

# 2013-2019, WITH covariates -----------------------------------------

# Subsetting tracts by racial majority

maj_bl <- violations %>% 
  filter(pct_bl_17 > 0.5) # 35,420 obs

maj_bl %>% 
  group_by(treat_yr_mo) %>% 
  summarize(n_tracts = n_distinct(geoid)) %>% 
  adorn_totals()
  # Cohort 1: 73
  # Cohort 2: 15
  # Cohort 3: 22
  # Cohort 4: 28
  # Cohort 5: 322
  # Total: 460

maj_wh <- violations %>% 
  filter(pct_wh_17 > 0.5) # 48,202 obs

maj_wh %>% 
  group_by(treat_yr_mo) %>% 
  summarize(n_tracts = n_distinct(geoid)) %>% 
  adorn_totals()
  # Cohort 1: 1
  # Cohort 2: 21
  # Cohort 3: 16 
  # Cohort 4: 3
  # Cohort 5: 585
  # Total: 626

maj_poc <- violations %>% 
  filter(pct_wh_17 < 0.5) # 103,488 obs

maj_poc %>% 
  group_by(treat_yr_mo) %>% 
  summarize(n_tracts = n_distinct(geoid)) %>% 
  adorn_totals()
# Cohort 1: 131
# Cohort 2: 58
# Cohort 3: 67 
# Cohort 4: 70
# Cohort 5: 1018
# Total: 1344
  
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
      title = "Figure 3. Tract-Level Group-Time ATE (With Covariates), 2013-2019",
      grtitle = "Received UA",
      xgap = 12,
      theming = FALSE) +
  theme_ipsum_rc(grid = FALSE, ticks = TRUE) +
  scale_x_continuous(breaks = seq(1,77,6),
                     labels = c("Jan 2013",
                                "Jul 2013",
                                "Jan 2014",
                                "Jul 2014",
                                "Jan 2015",
                                "Jul 2015",
                                "Jan 2016",
                                "Jul 2016",
                                "Jan 2017",
                                "Jul 2017",
                                "Jan 2018",
                                "Jul 2018",
                                "Jan 2019")) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, 
                                   margin = margin(t = 20)))

group_effects <- aggte(est_w_covars, type = "group")
summary(group_effects) # Wave 1 has a statistically significant bump in hhw violations 
ggdid(group_effects,
      title = "Fig. 5: Average Effect by Group (With Covariates)",
      theming = FALSE) +
  theme_ipsum_rc(grid = F) +
  theme(legend.position = "none")
   # All: Wave 1 has a +0.34 increase (ss), 2 has -0.20 (nss), 3 has -0.06(nss)
   # Maj Wh: Wave 1 -0.01(nss), Wave 2 -0.27(nss), Wave 3 dropped entirely
   # Maj POC: Wave 1 +0.31 (ss), 2 has -0.26 (nss), 3 has -0.12(nss)

group_effects <- aggte(est_w_covars, type = "dynamic")
summary(group_effects) # results not statistically significant
ggdid(group_effects,
      title = "Fig. 4. Average Effect by Tracts' Length of Exposure",
      xlab = "Pre/Post Treatment Time in Months",
      xgap = 12,
      theming = FALSE) +
  theme_ipsum_rc(grid = F)

group_effects <- aggte(est_w_covars, type = "simple")
summary(group_effects) # results not statistically significant
  #Maj Wh: -0.07 (nss)
  #Maj POC: +0.11 (nss)
  #Maj Bl: +0.15
  #All: +0.13

group_effects <- aggte(est_w_covars, type = "calendar")
summary(group_effects) # results not statistically significant

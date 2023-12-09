# -----------------------------------------------------------------------------
# Capstone Team 4: Exploratory Data Analysis
# 2012 - 2023 Utility-Related Violations
# 2023-12-08
# SL
# -----------------------------------------------------------------------------


# Dependent Packages ----------------------------------------------------------

library(tidyverse) # data manipulation
library(vtable) # summary stats in RMarkdown
library(kableExtra) # prettier tables
library(hrbrthemes) # prettier charts
library(viridis) # accessible color palettes
library(gghighlight) # visual emphasis on particular variables



# Load Data -------------------------------------------------------------------

zips <- read_csv("data_build/2012_2020_zip_set_for_analysis.csv.gz")  # 17,640 zip/yearmonth obs spanning Jan 2012 - Feb 2020
violations <- read.csv("data_build/cleaner_utility_violations.csv.gz") %>% 
  filter(inspectiondate <= "2020-02-29")   # 126,238 distinct violation obs spanning Jan. 2012 - Feb. 2020

# Descriptive Stats -----------------------------------------------------------

# Simple ZIP Code Counts by treatment cohort:

zips %>% 
  group_by(cohort, treated) %>% 
  summarize(n_months_treated = n_distinct(inspection_yr_mo),
            n_zip_codes = n_distinct(zip)) %>% 
  pivot_wider(names_from = treated, values_from = n_months_treated) 

# Summary Table, By Treatment Cohort:

# I did this in an R Notebook w/ html output; will put in our Github Markdown when finalized
zips %>% 
  ungroup() %>% 
  mutate(cohort = factor(cohort),
         treated = factor(treated)) %>% 
  st(group = 'cohort',
     group.test = T,
     col.align = 'center',
     title = 'Table 1. Summary Statistics by RTC Treatment Cohort: NYC Renter-Occupied Units, January 2012 - February 2020',
     out = 'browser')

# By ZIP & Year/Month

zips %>% 
  ungroup() %>% 
  select(-c(med_hh_inc_rou, med_gross_rent, tot_college_degree_rou, tot_wh_rou, tot_bl_rou, tot_asn_rou, tot_ltx_rou)) %>% 
  mutate(cohort = factor(cohort),
         treated = factor(treated)) %>% 
  st(col.align = 'center',
     title = 'Table 1. Summary Statistics by ZIP Code: NYC Renter-Occupied Units, January 2012 - February 2020',
     out = 'browser')


# Quick Descriptive Visuals: Violation Counts ---------------------------------------

# Monthly violation count visual (citywide) 
zips %>% 
  group_by(inspection_yr_mo) %>% 
  summarize(n_violations = sum(n_violations)) %>% 
  ggplot(aes(x = inspection_yr_mo,
             y = n_violations)) +
  geom_col(alpha = .8) +
  labs (x = NULL, y = "Citywide heat, hot water, or gas supply violations",
        title = "Monthly utility violations, 2012-20",
        subtitle = "Lorem ipsum dolor sit amet.",
        caption = "Source: NYC HPD Housing & Maintenance Code Violations, retrieved via NYC OpenData",
        fill = NULL)+
  theme_ipsum_rc(grid = "Y")

# Monthly violation intensity, by RTC treatment cohort 
# see plots/monthly_violation_intensity_by_cohort.png

cohort_labs <- c("1" = "RTC Wave 1 [n = 10 ZIPs]", "2" = "RTC Wave 2 [n = 5 ZIPs]", "3" = "RTC Wave 3 [n = 5 ZIPs]", "4" = "RTC Wave 4 [n = 5 ZIPs]", "5" = "RTC Control [n = 155 ZIPs]")

# as faceted columns chart:

zips %>% 
  mutate(cohort = factor(cohort)) %>% 
  group_by(cohort, inspection_yr_mo) %>% 
  summarize(avg_monthly_intensity = mean(n_violations_per_1k_units, na.rm = T),
            treated = mean(treated, na.rm = T)) %>%
  mutate(treated = factor(treated)) %>% 
  ggplot(aes(x = inspection_yr_mo,
             y = avg_monthly_intensity,
             group = cohort,
             fill = treated)) +
  geom_col() +
  scale_fill_manual(values = c("azure4", "#E1AF00"),
                    labels = c("Not Yet Treated", "Treated"))+
  facet_wrap(~cohort,
             scales = "free_x",
             labeller = labeller(cohort = cohort_labs)) +   
  labs (x = NULL, y = "hhw violations per 1k renter occupied units in ZIP",
        title = "Mean monthly utility violations per 1k renter occupied units in New York City ZIP codes",
        subtitle = "Issued by New York City housing inspectors, Jan. 2012 - Feb. 2020 ",
        caption = "Source: NYC HPD Housing & Maintenance Code Violations, retrieved via NYC OpenData",
        fill = NULL)+
  theme_ipsum_rc(grid = "Y")+
  theme(legend.position="top", legend.title = element_blank())

# as faceted line chart:

zips %>%
  mutate(cohort = factor(cohort)) %>% 
  group_by(cohort, inspection_yr_mo) %>% 
  summarize(avg_monthly_intensity = mean(n_violations_per_1k_units, na.rm = T)) %>%
  ggplot(aes(x = inspection_yr_mo,
             y = avg_monthly_intensity,
             group = cohort, 
             color = cohort)) +
  geom_line(alpha = .6, linewidth = 1) +
  gghighlight(use_direct_label = F,
              unhighlighted_params = list(linewidth = 0.5, alpha = .7))+
  facet_wrap(~cohort,
             scales = "free_x",
             labeller = labeller(cohort = cohort_labs)) +  
  scale_color_viridis(discrete = T)+
  labs (x = NULL, y = "hhw violations per 1k renter occupied units in ZIP",
        title = "Mean monthly utility violations per 1k renter occupied units in New York City ZIP codes",
        subtitle = "Issued by New York City housing inspectors, Jan. 2012 - Feb. 2020 ",
        caption = "Source: NYC HPD Housing & Maintenance Code Violations, retrieved via NYC OpenData",
        fill = NULL)+
  theme_ipsum_rc(grid = "Y")+
  theme(legend.position="none", legend.title = element_blank())



# Monthly violation count visual, by RTC treatment cohort 
zips %>% 
  ggplot(aes(x = inspection_yr_mo,
             y = n_violations)) +
  geom_line() +
  facet_wrap(~cohort) +   # x axis labelling screwed up 
  labs (x = NULL, y = "Citywide heat, hot water, or gas supply violations",
        title = "Monthly utility violations by cohort, 2012-23",
        subtitle = "Lorem ipsum dolor sit amet.",
        caption = "Source: NYC HPD Housing & Maintenance Code Violations, retrieved via NYC OpenData",
        fill = NULL)+
  theme_ipsum_rc(grid = "Y") 

# Yearly violation count visual
precovid_violations %>% 
  group_by(inspection_yr) %>% 
  summarize(tot_violations = n_distinct(violationid, na.rm = T)) %>% 
  ggplot(aes(x = inspection_yr,
             y = tot_violations)) +
  geom_col(alpha = .8) +
  scale_x_continuous(breaks = c(2012, 2016, 2020, 2023))+
  labs (x = NULL, y = "Citywide heat, hot water, or gas supply violations",
        title = "Yearly NYC Utility Violation Counts, 2012 - 23",
        subtitle = "Lorem ipsum dolor sit amet.",
        caption = "Source: NYC HPD Housing & Maintenance Code Violations, retrieved via NYC OpenData",
        fill = NULL)+
  theme_ipsum_rc(grid = "Y")

# Yearly violation count visual, by RTC treatment cohort
precovid_violations %>% 
  group_by(cohort, inspection_yr, treated) %>% 
  summarize(tot_violations = n_distinct(violationid, na.rm = T)) %>% 
  mutate(treated = factor(treated)) %>% 
  ggplot(aes(x = inspection_yr,
             y = tot_violations,
             color = treated)) +
  labs (x = NULL, y = "Citywide heat, hot water, or gas supply violations",
        title = "Yearly Violation Counts, by Treatment Status",
        subtitle = "Lorem ipsum dolor sit amet.",
        caption = "Source: NYC HPD Housing & Maintenance Code Violations, retrieved via NYC OpenData",
        fill = NULL)+
  scale_x_continuous(breaks = c(2012, 2016, 2020))+
  scale_color_manual(values = c("azure3", "#21918c"), labels = c("Before RTC", "After RTC"))+
  facet_wrap(~cohort) +
  geom_line(linewidth = 1) +
  theme_ipsum_rc(grid = F)+
  theme(legend.position = "top")

# Yearly visual, by treatment status
precovid_violations %>% 
  group_by(treated, inspection_yr) %>% 
  summarize(tot_violations = n_distinct(violationid, na.rm = T)) %>% 
  ggplot(aes(x = inspection_yr,
             y = tot_violations)) +
  facet_grid(~treated) +
  geom_col() +
  scale_fill_viridis_d()+
  labs (x = NULL, y = "Heat, hot water, or gas supply violations",
        title = "Annual NYC Utility Violation Counts, 2012-23",
        subtitle = "Lorem ipsum dolor sit amet.",
        caption = "Source: NYC HPD Housing & Maintenance Code Violations, retrieved via NYC OpenData",
        fill = NULL)+
  theme_ipsum_rc(grid = F)

# Yearly violations, by treatment status
zip_intensity %>% 
  group_by(treated, inspection_yr) %>% 
  summarize(tot_violations = n_distinct(violationid, na.rm = T)) %>% 
  mutate(treated = factor(treated)) %>% 
  ggplot(aes(x = inspection_yr,
             y = tot_violations,
             fill = treated)) +
  geom_col(position = "stack") +
  scale_fill_viridis_d(alpha = .8, labels = c("Before RTC", "After RTC"))+
  scale_x_continuous(breaks = c(2012:2023))+
  labs (x = NULL, y = "Heat, hot water, or gas supply violations",
        title = "Yearly Utility Violation Counts, by Treatment Status",
        subtitle = "Lorem ipsum dolor sit amet.",
        caption = "Source: NYC HPD Housing & Maintenance Code Violations, retrieved via NYC OpenData",
        fill = NULL)+
  theme_ipsum_rc(grid = "Y")+
  theme(legend.position = "top")


# Line graph of violation counts, by cohort (annual)
precovid_violations %>% 
  group_by(cohort, inspection_yr) %>% 
  summarize(tot_violations = n_distinct(violationid, na.rm = T)) %>%   
  mutate(cohort = factor(cohort)) %>% 
  ggplot(aes(x = inspection_yr, 
             y = tot_violations, 
             group = cohort, 
             color = cohort))+
  labs (x = NULL, y = "Heat, hot water, or gas supply violations",
        title = "Yearly Utility Violation Counts by RTC Cohort",
        subtitle = "Lorem ipsum dolor sit amet.",
        caption = "Source: NYC HPD Housing & Maintenance Code Violations, retrieved via NYC OpenData",
        fill = NULL)+
  scale_colour_viridis_d(direction = -1)+
  geom_line(linewidth = 1.5,
            alpha = .6) +
  scale_x_continuous(breaks = c(2012, 2016, 2017, 2018, 2019, 2021, 2023))+
  theme_ipsum_rc(grid = "X")

# Line graph of violation intensity, by cohort (annual)
precovid_violations %>% 
  group_by(cohort, inspection_yr) %>% 
  summarize(avg_yr_intensity = mean(yr_zip_violations_per1k_units, na.rm = T)) %>%   
  mutate(cohort = factor(cohort)) %>% 
  ggplot(aes(x = inspection_yr, 
             y = avg_yr_intensity, 
             group = cohort, 
             color = cohort))+
  labs (x = NULL, y = "Heat, hot water, or gas supply violations",
        title = "Yearly Utility Violation Counts by RTC Cohort",
        subtitle = "Lorem ipsum dolor sit amet.",
        caption = "Source: NYC HPD Housing & Maintenance Code Violations, retrieved via NYC OpenData",
        fill = NULL)+
  scale_colour_viridis_d(direction = -1)+
  geom_line(linewidth = 1.5,
            alpha = .6) +
  scale_x_continuous(breaks = c(2012, 2016, 2017, 2018, 2019, 2021, 2023))+
  theme_ipsum_rc(grid = "X")


precovid_violations %>%
  mutate(ever_treated = factor(
    case_when(
      cohort < 5 ~ 1,
      TRUE ~ 0 ))
    )%>% 
  group_by(ever_treated, inspection_yr_mo) %>% 
  summarize(avg_monthly_intensity = mean(month_zip_violations_per1k_units, na.rm = T)) %>%   
  ggplot(aes(x = inspection_yr_mo, 
             y = avg_monthly_intensity, 
             group = ever_treated, 
             color = ever_treated))+
  labs (x = NULL, y = "Heat, hot water, or gas supply violations",
        title = "Yearly Utility Violation Counts, by RTC Status",
        subtitle = "Lorem ipsum dolor sit amet.",
        caption = "Source: NYC HPD Housing & Maintenance Code Violations, retrieved via NYC OpenData",
        fill = NULL)+
  scale_colour_viridis_d(direction = -1)+
  geom_line(linewidth = 1.5,
            alpha = .6) +
 # scale_x_continuous(breaks = c(2012, 2016, 2017, 2018, 2019, 2021, 2023))+
  theme_ipsum_rc(grid = "X")

# Monthly intensity, by ever-treated status(columns)
zips %>%
  mutate(ever_treated = factor(
    case_when(
    cohort < 5 ~ 1,
    TRUE ~ 0 ))
    )%>% 
  group_by(ever_treated, inspection_yr_mo) %>% 
  summarize(avg_monthly_intensity = mean(n_violations_per_1k_units, na.rm = T)) %>%
  ggplot(aes(x = inspection_yr_mo,
             y = avg_monthly_intensity,
             group = ever_treated, 
             fill = ever_treated)) +
  geom_col(alpha = .6) +
  scale_fill_viridis(discrete = T) + 
  labs (x = NULL, y = "hhw violations per 1k renter-occupied units in ZIP",
        title = "Monthly utility violations by RTC Treatment Status, Jan. 2012 - Feb. 2020",
        subtitle = "Lorem ipsum dolor sit amet.",
        caption = "Source: NYC HPD Housing & Maintenance Code Violations, retrieved via NYC OpenData",
        fill = NULL)+
  theme_ipsum_rc(grid = "Y")



# Monthly intensity, by cohort (illegible spaghetti chart)
zips %>%
  mutate(cohort = factor(cohort)) %>% 
  group_by(cohort, inspection_yr_mo) %>% 
  summarize(avg_monthly_intensity = mean(n_violations_per_1k_units, na.rm = T)) %>%
  ggplot(aes(x = inspection_yr_mo,
             y = avg_monthly_intensity,
             group = cohort, 
             color = cohort)) +
  geom_line(alpha = .8) +
  labs (x = NULL, y = "Citywide heat, hot water, or gas supply violations",
        title = "Monthly utility violations, 2012-23",
        subtitle = "Lorem ipsum dolor sit amet.",
        caption = "Source: NYC HPD Housing & Maintenance Code Violations, retrieved via NYC OpenData",
        fill = NULL)+
  theme_ipsum_rc(grid = "Y")

# Monthly intensity, by cohort (columns)
zips %>%
  mutate(cohort = factor(cohort)) %>% 
  group_by(cohort, inspection_yr_mo) %>% 
  summarize(avg_monthly_intensity = mean(n_violations_per_1k_units, na.rm = T)) %>%
  ggplot(aes(x = inspection_yr_mo,
             y = avg_monthly_intensity,
             group = cohort, 
             fill = cohort)) +
  geom_col(alpha = .8,
           position = "identity") +
  scale_fill_viridis(discrete = T)+
  labs (x = NULL, y = "avg hhw violations per 1k renter occupied units in ZIP",
        title = "Monthly utility violations, 2012-23",
        subtitle = "Lorem ipsum dolor sit amet.",
        caption = "Source: NYC HPD Housing & Maintenance Code Violations, retrieved via NYC OpenData",
        fill = NULL)+
  theme_ipsum_rc(grid = "Y")




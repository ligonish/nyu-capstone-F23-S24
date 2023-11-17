# -----------------------------------------------------------------------------
# Capstone Team 4: Exploratory Data Analysis
# 2012 - 2023 Utility-Related Violations
# 2023-11-15
# SL
# -----------------------------------------------------------------------------


# Dependent Packages ----------------------------------------------------------

library(tidyverse) # data manipulation
library(hrbrthemes) # prettier charts
library(viridis) # accessible color palettes


# Load Data -------------------------------------------------------------------

zip_intensity <- read_csv("data_build/rough_annual_zip_violation_intensities.csv.gz")


# Quick Summary Stats: Violation Counts ---------------------------------------

# Monthly violation count, by RTC treatment cohort
util_summary <- zip_intensity %>% 
  group_by(cohort, inspection_yr_mo) %>% 
  summarize(tot_violations = n_distinct(violationid, na.rm = T)) 

# Monthly violation count visual, by RTC treatment cohort 
util_summary %>% 
  ggplot(aes(x = inspection_yr_mo,
             y = tot_violations)) +
  geom_col(alpha = .8) +
  labs (x = NULL, y = "Citywide heat, hot water, or gas supply violations",
        title = "Monthly utility violations, 2012-23",
        subtitle = "Lorem ipsum dolor sit amet.",
        caption = "Source: NYC HPD Housing & Maintenance Code Violations, retrieved via NYC OpenData",
        fill = NULL)+
  theme_ipsum_rc(grid = "Y")

# Monthly violation count visual, by RTC treatment cohort 
util_summary %>% 
  ggplot(aes(x = inspection_yr_mo,
             y = tot_violations)) +
  geom_line() +
  facet_wrap(~cohort) +   # x axis labelling screwed up 
  labs (x = NULL, y = "Citywide heat, hot water, or gas supply violations",
        title = "Monthly utility violations by cohort, 2012-23",
        subtitle = "Lorem ipsum dolor sit amet.",
        caption = "Source: NYC HPD Housing & Maintenance Code Violations, retrieved via NYC OpenData",
        fill = NULL)+
  theme_ipsum_rc(grid = "Y") 

# Yearly violation count visual
zip_intensity %>% 
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
zip_intensity %>% 
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
zip_intensity %>% 
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


# Line graph, by cohort (annual)
zip_intensity %>% 
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








